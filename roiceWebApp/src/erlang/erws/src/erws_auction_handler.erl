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
    CurrentTime = erlang:system_time(seconds),
    if
        %% Check if the auction is expired, in case the supervisor tries to restart it after its end
        CurrentTime > EndDate ->
            logger:info("[erws_auction_handler] auction_handle => Auction ended"),
            auction_receive(Phone, Bid, 0, EndDate);
        true ->
            %% Auction is still live
            AuctionPid = self(),
            logger:info("[erws_auction_handler] auction_handle => Auction process started with pid: ~p~n", [AuctionPid]),
            erws_mnesia:save_auction(Phone, AuctionPid),
            logger:info("[erws_auction_handler] auction_handle => Auction saved for the phone: ~p ~n", [Phone]),
            Text = "Live auctions update requested!",
            gproc:send({p, l, {?AGENT, {live_auctions}}}, {live_auctions_update, Text}),

            case erws_mnesia:get_winner_bidder(Phone) of
                %% When auction_handle is called for the first time
                not_found ->
                    auction_receive(Phone, Bid, AuctionTime, EndDate);
                %% When the supervisor calls again the function to restart a child auction process
                {_WinnerEmail, CurrentBid} ->
                    auction_receive(Phone, CurrentBid, AuctionTime, EndDate)
            end
    end.

%% Handle auction messages
auction_receive(Phone, Bid, AuctionTime, EndDate) ->
    receive
    %% Receive JOIN request from a Bidder
        {bidder_join, PhoneName} ->
            RemainingTime = get_time_remaining(EndDate),
            CurrentWinner = erws_mnesia:get_winner_bidder(PhoneName),
            logger:info("[erws_auction_handler] auction_receive => Current winner for the phone ~p: ~p~n", [PhoneName, CurrentWinner]),
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
            logger:info("[erws_auction_handler] auction_receive => Current winner for the phone ~p: ~p~n", [PhoneName, CurrentWinner]),
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
                    logger:info("[erws_auction_handler] auction_receive => Bid of ~p from ~p for ~p saved~n", [NewBid, BidderEmail, PhoneName]),
                    auction_receive(Phone, NewBid, EndDate - erlang:system_time(second), EndDate);
                true ->
                    gproc:send({p, l, {?AGENT, PhoneName}}, {no_bid, Bid, RemainingTime}),
                    auction_receive(Phone, Bid, EndDate - erlang:system_time(second), EndDate)
            end

    %% End of the auction
    after AuctionTime * 1000 -> % Convert DelayInSeconds to milliseconds
        %% Init the HTTP message to send to the /handleWinnerMessage endpoint
        {ok, Ip} = application:get_env(tomcat_server_IP),
        URL = uri_string:normalize("http://" ++ Ip ++ ":8080/handleAuctionEnd"),
        logger:debug("[erws_auction_handler] auction_receive => The URL is: ~p ~n", [URL]),
        ContentType = "application/json",
        HttpOptions = [],
        Options = [],

        case erws_mnesia:get_winner_bidder(Phone) of
            %% No bidders case
            not_found ->
                logger:info("[erws_auction_handler] auction_receive => No bidders for the auction of the phone: ~p~n", [Phone]),
                RemainingTime = get_time_remaining(EndDate),
                Response = <<"No bidders">>,
                gproc:send({p, l, {?AGENT, Phone}}, {no_bidders, Response, RemainingTime}),

                Body = jsone:encode(
                    #{
                        <<"winner">> => Response,
                        <<"phone">> => Phone
                    });

            %% Winner bidder case
            {WinnerEmail, WinningBid} ->
                RemainingTime = get_time_remaining(EndDate),
                logger:info("[erws_auction_handler] auction_receive => Phone: ~p, Winner: ~p, Winning Bid: ~p~n", [Phone, WinnerEmail, WinningBid]),
                gproc:send({p, l, {?AGENT, Phone}}, {winner_bidder, Phone, WinnerEmail, WinningBid, RemainingTime}),
                erws_mnesia:delete_bid(Phone),
                logger:info("[erws_auction_handler] auction_receive => Winner Bid deleted for the phone: ~p~n", [Phone]),

                Body = jsone:encode(
                    #{
                        <<"winner">> => WinnerEmail,
                        <<"winningBidValue">> => WinningBid,
                        <<"phone">> => Phone
                    })
        end,
        %% Delete the auction from Mnesia in both cases
        erws_mnesia:delete_auction(Phone),
        logger:info("[erws_auction_handler] auction_receive => Auction deleted for the phone: ~p~n", [Phone]),

        %% Send the HTTP request to the Tomcat server
        case httpc:request(post, {URL, [], ContentType, Body}, HttpOptions, Options) of
            {ok, {{_, StatusCode, _}, _, ResponseBody}} ->
                logger:info("[erws_auction_handler] auction_receive => Request executed, status Code: ~p. Response Body: ~p~n", [StatusCode, ResponseBody]);
            {error, Reason} ->
                logger:error("[erws_auction_handler] auction_receive => Request failed. Reason: ~p~n", [Reason])
        end
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