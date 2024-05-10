%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erws_auction_handler).

%% gen_server callbacks
-export([auction_handle/4, start_link/4]).

% Name of the agent module
-define(AGENT, erws_auction_agent).

% Spawn a new auction process executing auction_handle
start_link(Phone, Bid, AuctionTime, EndDate) ->
    Pid = spawn_link(?MODULE, auction_handle, [Phone, Bid, AuctionTime, EndDate]),
    {ok, Pid}.

%% Init auction process
auction_handle(Phone, Bid, AuctionTime, EndDate) ->
    AuctionPid = self(),
    logger:debug("[erws_auction_handler] auction_handle => Auction process started with pid: ~p~n", [AuctionPid]),
    erws_mnesia:save_auction(Phone, AuctionPid),
    Text = "Live auctions update requested!",
    gproc:send({p, l, {?AGENT, {live_auctions}}}, {live_auctions_update, Text}),
    auction_receive(Phone, Bid, AuctionTime, EndDate).
%%    erws_dynamic_sup:terminate_auction_process(self()).

%% Handle auction messages
auction_receive(Phone, Bid, AuctionTime, EndDate) ->
    receive
    %% Receive JOIN request from a Bidder
        {bidder_join, PhoneName} ->
            RemainingTime = get_time_remaining(EndDate),
            CurrentWinner = erws_mnesia:get_winner_bidder(PhoneName),
            case CurrentWinner of
                {WinnerEmail, _} ->
                    gproc:send({p, l, {?AGENT, PhoneName}}, {joined, Bid, RemainingTime, WinnerEmail});
                not_found ->
                    gproc:send({p, l, {?AGENT, PhoneName}}, {joined, Bid, RemainingTime, []})
            end,
            auction_receive(Phone, Bid, EndDate - erlang:system_time(second), EndDate);

    %% Receive TIMER message from a Bidder
        {timer, PhoneName} ->
            RemainingTime = get_time_remaining(EndDate),
            CurrentWinner = erws_mnesia:get_winner_bidder(PhoneName),
            case CurrentWinner of
                {WinnerEmail, _} ->
                    gproc:send({p, l, {?AGENT, PhoneName}}, {send_timer, Bid, RemainingTime, WinnerEmail});
                not_found ->
                    gproc:send({p, l, {?AGENT, PhoneName}}, {send_timer, Bid, RemainingTime, []})
            end,
            auction_receive(Phone, Bid, EndDate - erlang:system_time(second), EndDate);

    %% Receive BID message from a Bidder
        {send, BidderEmail, NewBid, HandlerPid, PhoneName} ->
            logger:info("[erws_auction_handler] auction_receive => email: ~p, NewBid: ~p, Bid: ~p, From: ~p ~n", [BidderEmail, NewBid, Bid, HandlerPid]),
            RemainingTime = get_time_remaining(EndDate),
            if
                Bid < NewBid ->
                    logger:info("[erws_auction_handler] auction_receive => NewBid: ~p > Current Bid: ~p!~n", [NewBid, Bid]),
                    logger:debug("[erws_auction_handler] auction_receive => Send update bid to bidders"),
                    gproc:send({p, l, {?AGENT, PhoneName}}, {new_bid, NewBid, RemainingTime, BidderEmail}),
                    erws_mnesia:save_bid(PhoneName, BidderEmail, NewBid),
                    auction_receive(Phone, NewBid, EndDate - erlang:system_time(second), EndDate);
                true ->
                    gproc:send({p, l, {?AGENT, PhoneName}}, {no_bid, Bid, RemainingTime}),
                    auction_receive(Phone, Bid, EndDate - erlang:system_time(second), EndDate)
            end
    after AuctionTime * 1000 -> % Convert DelayInSeconds to milliseconds
        case erws_mnesia:get_winner_bidder(Phone) of
            not_found ->
                logger:info("[erws_auction_handler] auction_receive => No bidders for the auction of the phone: ~p~n", [Phone]),
                RemainingTime = get_time_remaining(EndDate),
                Response = <<"No bidders">>,
                gproc:send({p, l, {?AGENT, Phone}}, {no_bidders, Response, RemainingTime});
            {WinnerEmail, WinningBid} ->
                RemainingTime = get_time_remaining(EndDate),
                logger:info("[erws_auction_handler] auction_receive => Phone: ~p, Winner: ~p, Winning Bid: ~p", [Phone, WinnerEmail, WinningBid]),
                gproc:send({p, l, {?AGENT, Phone}}, {winner_bidder, Phone, WinnerEmail, WinningBid, RemainingTime})
        end,
        erws_mnesia:delete_auction(Phone)
    end.


% Function to compute the auction remaining time
get_time_remaining(EndDate) ->
    % Compute the remaining time in seconds
    RemainingTimeSeconds = EndDate - erlang:system_time(second),
    % Convert the remaining time in a readable format
    RemainingDays = RemainingTimeSeconds div 86400,
    RemainingHours = (RemainingTimeSeconds rem 86400) div 3600,
    RemainingMinutes = ((RemainingTimeSeconds rem 86400) rem 3600) div 60,
    RemainingSeconds = (RemainingTimeSeconds rem 60),
    % Format the remaining time in a readable string
    TimeRemainingString = io_lib:format("~b d ~b h ~b m ~b s", [RemainingDays, RemainingHours, RemainingMinutes, RemainingSeconds]),
    TimeRemainingString.