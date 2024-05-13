%%%-------------------------------------------------------------------
%%% @doc
%%%   erws_auction_agent module handles WebSocket communication for auctions.
%%%   It initializes WebSocket connections, handles requests, and manages
%%%   incoming WebSocket text frames. This module also processes actions related
%%%   to auctions, like new auctions, joining auctions, sending bids, and
%%%   getting auction timer updates.
%%% @end
%%%-------------------------------------------------------------------

-module(erws_auction_agent).

-export([
    init/2,
    websocket_init/3,
    handle/2,
    terminate/3,
    receive_joined/1,
    websocket_handle/2,
    websocket_info/2,
    websocket_terminate/3
]).

% Initialize WebSocket connection
init(Req, State) ->
    {cowboy_websocket, Req, State}.

websocket_init(_TransportName, Req, _Opts) ->
    {ok, Req, _TransportName}.

% Handle HTTP requests agent
handle(Req, State) ->
    logger:debug("[erws_auction_agent] handle => Request not expected: ~p~n", [Req]),
    {ok, Req2} = cowboy_http_req:reply(404, [{'Content-Type', <<"text/html">>}]),
    {ok, Req2, State}.

% Handles incoming WebSocket text frames
websocket_handle(Frame = {text, Message}, State) ->
    logger:debug("[erws_auction_agent] websocket_handle => Frame: ~p, State: ~p~n", [Frame, State]),
    logger:debug("[erws_auction_agent] websocket_handle => Received ~p~n", [Frame]),

    DecodedMessage = jsone:try_decode(Message),

    Response = case element(1, DecodedMessage) of
                   ok ->
                       Json = element(2, DecodedMessage),
                       logger:debug("[erws_auction_agent] websocket_handle => Decoded ~p~n", [Json]),
                       handle_websocket_frame(Json, State);
                   error ->
                       logger:error("[erws_auction_agent] websocket_handle => Failed to decode JSON: ~p~n", [Message]),
                       {ok, State}
               end,
    Response.

% Handle a frame after JSON decoding
handle_websocket_frame(Map, State) ->
    logger:debug("[erws_auction_agent] handle_websocket_frame => Map is ~p~n", [Map]),
    Action = maps:get(<<"action">>, Map),
    logger:info("[erws_auction_agent] handle_websocket_frame => Action: ~p~n", [Action]),

    case Action of
        <<"new_auction">> -> % Handle new auction action
            handle_new_auction(Map);

        <<"join_auction">> ->
            BidderEmail = maps:get(<<"email">>, Map),
            logger:debug("[erws_auction_agent] handle_websocket_frame => EMAIL of the new join user: ~p~n", [BidderEmail]),
            handle_join_auction(Map, State);

        <<"live_auctions">> ->
            handle_live_auctions(State);

        <<"send">> ->
            handle_send_bid(Map, State);

        <<"timer">> ->
            handle_get_timer(Map, State);

        _ ->
            logger:error("[erws_auction_agent] handle_websocket_frame => Unknown action: ~p~n", [Action]),
            {ok, State}
    end.

handle_new_auction(Map) ->
    StartDate = maps:get(<<"startSeconds">>, Map),
    EndDate = maps:get(<<"endSeconds">>, Map),
    case is_integer(StartDate) andalso is_integer(EndDate) of
        true ->
            logger:info("[erws_auction_agent] handle_new_auction => New auction scheduled for ~p~n", [StartDate]),
            CurrentDate = erlang:system_time(second),
            Delay = StartDate - CurrentDate,
            AuctionTime = EndDate - StartDate,
            case Delay > 0 of
                true ->
                    timer:sleep(Delay * 1000),
                    PhoneName = maps:get(<<"phoneName">>, Map),
                    erws_mnesia:delete_auction(PhoneName),
                    erws_mnesia:delete_bid(PhoneName),
                    MinimumPrice = maps:get(<<"minimumPrice">>, Map),
                    erws_dynamic_sup:start_auction_process(PhoneName, MinimumPrice, AuctionTime, EndDate),
                    AuctionPid = erws_mnesia:get_auction_pid(PhoneName),
                    {ok, AuctionPid}; % Return the tuple {ok, AuctionPid}
                false ->
                    logger:error("[erws_auction_agent] handle_new_auction => Start date has already passed, cannot spawn auction process. \n"),
                    undefined % Return undefined or any other value to indicate failure
            end;
        false ->
            logger:error("[erws_auction_agent] handle_new_auction => Invalid start date \n"),
            undefined % Return undefined or any other value to indicate failure
    end.

handle_join_auction(Map, State) ->
    PhoneName = maps:get(<<"phoneName">>, Map),
    AuctionPid = erws_mnesia:get_auction_pid(PhoneName),
    init_bidder(AuctionPid, PhoneName),
    gproc:reg({p, l, {?MODULE, PhoneName}}),
    Pid = gproc:lookup_pids({p, l, {?MODULE, PhoneName}}),
    logger:info("[erws_auction_agent] handle_join_auction => WEBSOCKET_INIT process started here: ~p~n", [Pid]),
    receive_joined(State).


handle_live_auctions(State) ->
    gproc:reg({p, l, {?MODULE, {live_auctions}}}),
    logger:info("[erws_auction_agent] handle_live_auctions => Client registered to the live auctions session! \n"),
    receive
        {live_auctions_update, Text} ->
            Response = io_lib:format("~p", [Text]),
            {reply, {text, Response}, State, hibernate}
    end.

handle_send_bid(Map, State) ->
    % Get attributes from map
    PhoneName = maps:get(<<"phone_name">>, Map),
    BidderEmail = maps:get(<<"email">>, Map),
    BidDate = maps:get(<<"date">>, Map),
    BidValue = maps:get(<<"value">>, Map),
    logger:info("[erws_auction_agent] handle_send_bid => Successfully received bid from client: ~p~n, ~p~n, ~p~n, ~p~n", [PhoneName, BidderEmail, BidDate, BidValue]),

    AuctionPid = erws_mnesia:get_auction_pid(PhoneName),

    % Send message with the bid to the auction process
    process_bid(AuctionPid, BidderEmail, BidValue, self(), PhoneName),

    receive
        {new_bid, NewBid, RemainingTime, CurrentWinner} ->
            logger:info("[erws_auction_agent] handle_send_bid => Received New Bid \n"),
            Response = io_lib:format("Bid:~p RemainingTime:~p CurrentWin:~p", [NewBid, RemainingTime, CurrentWinner]),
            {reply, {text, Response}, State, hibernate};
        {no_bid, Bid} ->
            logger:info("[erws_auction_agent] handle_send_bid => Received Bid < Current Max Bid \n"),
            Response = io_lib:format("Bid:~p", [Bid]),
            {reply, {text, Response}, State, hibernate};
        {winner_bidder, Phone, WinnerEmail, WinningBid, RemainingTime} ->
            logger:info("[erws_auction_agent] handle_send_bid => Phone: ~p, Winner: ~p, Winning Bid: ~p~n", [Phone, WinnerEmail, WinningBid]),
            Response = io_lib:format("Phone:~p Winner:~p Winning Bid:~p RemainingTime:~p", [Phone, WinnerEmail, WinningBid, RemainingTime]),
            {reply, {text, Response}, State, hibernate}
    end.


handle_get_timer(Map, State) ->
    PhoneName = maps:get(<<"phone_name">>, Map),
    AuctionPid = erws_mnesia:get_auction_pid(PhoneName),

    % Send timer request message to the auction process
    process_timer(AuctionPid, PhoneName),
    receive
        {send_timer, Bid, RemainingTime, CurrentWinner} ->
            logger:debug("[erws_auction_agent] handle_get_timer => Get Auction Timer \n"),
            Response = io_lib:format("Bid:~p RemainingTime:~p CurrentWin:~p", [Bid, RemainingTime, CurrentWinner]),
            {reply, {text, Response}, State, hibernate}
    end.

receive_joined(State) ->
    receive
        {joined, Bid, RemainingTime, CurrentWinner} ->
            Response = io_lib:format("Bid:~p RemainingTime:~p CurrentWin:~p", [Bid, RemainingTime, CurrentWinner]),
            {reply, {text, Response}, State, hibernate};
        {not_joined} ->
            logger:info("[erws_auction_agent] receive_joined => Bidder not joined! \n"),
            {ok, State};
        {new_bid, NewBid, RemainingTime, CurrentWinner} ->
            logger:info("[erws_auction_agent] receive_joined => Received New Bid \n"),
            Response = io_lib:format("Bid:~p RemainingTime:~p CurrentWin:~p", [NewBid, RemainingTime, CurrentWinner]),
            {reply, {text, Response}, State, hibernate};
        {no_bid, Bid} ->
            logger:info("[erws_auction_agent] receive_joined => Received Bid < Current Max Bid \n"),
            Response = io_lib:format("Bid:~p", [Bid]),
            {reply, {text, Response}, State, hibernate};
        {no_bidders, Text, RemainingTime} ->
            logger:info("[erws_auction_agent] receive_joined => Auction terminated, no bidders \n"),
            Response = io_lib:format("Winner:~p RemainingTime:~p", [Text, RemainingTime]),
            {reply, {text, Response}, State, hibernate};
        {winner_bidder, Phone, WinnerEmail, WinningBid, RemainingTime} ->
            logger:info("[erws_auction_agent] receive_joined => Phone: ~p, Winner: ~p, Winning Bid: ~p~n", [Phone, WinnerEmail, WinningBid]),
            Response = io_lib:format("Phone:~p Winner:~p Winning Bid:~p RemainingTime:~p", [Phone, WinnerEmail, WinningBid, RemainingTime]),
            {reply, {text, Response}, State, hibernate};
        {send_timer, Bid, RemainingTime, CurrentWinner} ->
            logger:info("[erws_auction_agent] receive_joined => Get Auction Timer \n"),
            Response = io_lib:format("Bid:~p RemainingTime:~p CurrentWin:~p", [Bid, RemainingTime, CurrentWinner]),
            {reply, {text, Response}, State, hibernate}
    end.

%% Initialize bidder process - send to the auction agent bidder's information
init_bidder(AuctionPid, PhoneName) ->
    AuctionPid ! {bidder_join, PhoneName}.

% Function to process the bid received from a user
process_bid(AuctionPid, BidderEmail, BidValue, HandlerPid, PhoneName) ->
    AuctionPid ! {send, BidderEmail, BidValue, HandlerPid, PhoneName}.

% Function to process the get timer request from a user
process_timer(AuctionPid, PhoneName) ->
    AuctionPid ! {timer, PhoneName}.

% Terminate WebSocket connection
websocket_terminate(_Reason, _Req, _State) ->
    ok.

% Terminate function
terminate(_Reason, _Req, _State) ->
    ok.

websocket_info(Info, State) ->
    case Info of
        {joined, Bid, RemainingTime, CurrentWinner} ->
            Response = io_lib:format("[erws_auction_agent] websocket_info => Bid:~p RemainingTime:~p CurrentWin:~p", [Bid, RemainingTime, CurrentWinner]),
            {reply, {text, Response}, State, hibernate};
        {not_joined} ->
            logger:debug("[erws_auction_agent] websocket_info => Bidder not joined! \n"),
            {ok, State};
        {new_bid, NewBid, RemainingTime, CurrentWinner} ->
            logger:debug("[erws_auction_agent] websocket_info => Received New Bid \n"),
            Response = io_lib:format("Bid:~p RemainingTime:~p CurrentWin:~p", [NewBid, RemainingTime, CurrentWinner]),
            {reply, {text, Response}, State, hibernate};
        {no_bid, Bid} ->
            logger:debug("[erws_auction_agent] websocket_info => Received Bid < Current Max Bid \n"),
            Response = io_lib:format("Bid:~p", [Bid]),
            {reply, {text, Response}, State, hibernate};
        {no_bidders, Text, RemainingTime} ->
            logger:debug("[erws_auction_agent] websocket_info => Auction terminated, no bidders \n"),
            Response = io_lib:format("Winner:~p RemainingTime:~p", [Text, RemainingTime]),
            {reply, {text, Response}, State, hibernate};
        {winner_bidder, Phone, WinnerEmail, WinningBid, RemainingTime} ->
            logger:debug("[erws_auction_agent] websocket_info => Phone: ~p, Winner: ~p, Winning Bid: ~p~n", [Phone, WinnerEmail, WinningBid]),
            Response = io_lib:format("Phone:~p Winner:~p Winning Bid:~p RemainingTime:~p", [Phone, WinnerEmail, WinningBid, RemainingTime]),
            {reply, {text, Response}, State, hibernate};
        {send_timer, Bid, RemainingTime, CurrentWinner} ->
            logger:debug("[erws_auction_agent] websocket_info => Get Auction Timer \n"),
            Response = io_lib:format("Bid:~p RemainingTime:~p CurrentWin:~p", [Bid, RemainingTime, CurrentWinner]),
            {reply, {text, Response}, State, hibernate};
        {live_auctions_update, Text} ->
            Response = io_lib:format("~p", [Text]),
            {reply, {text, Response}, State, hibernate}
    end.
