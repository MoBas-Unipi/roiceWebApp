-module(erws_auction_agent).

-export([
    init/2,
    websocket_init/3,
    handle/2,
    terminate/3,
%%    auction_handle/1,
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

% Handle HTTP requestsagent
handle(Req, State) ->
    logger:debug("[handle] => Request not expected: ~p", [Req]),
    {ok, Req2} = cowboy_http_req:reply(404, [{'Content-Type', <<"text/html">>}]),
    {ok, Req2, State}.

%%% Handle auction messages
%%auction_handle([Phone, Bid, AuctionTime, EndDate]) ->
%%    erws_mnesia:save_auction(Phone, self()),
%%    auction_receive(Phone, Bid, AuctionTime, EndDate).
%%
%%auction_receive(Phone, Bid, AuctionTime, EndDate) ->
%%    receive
%%    %% Receive JOIN request from a Bidder
%%        {bidder_join, PhoneName} ->
%%            RemainingTime = get_time_remaining(EndDate),
%%            CurrentWinner = erws_mnesia:get_winner_bidder(PhoneName),
%%            case CurrentWinner of
%%                {WinnerEmail, _} ->
%%                    gproc:send({p, l, {?MODULE, PhoneName}}, {joined, Bid, RemainingTime, WinnerEmail});
%%                not_found ->
%%                    gproc:send({p, l, {?MODULE, PhoneName}}, {joined, Bid, RemainingTime, []})
%%            end,
%%            auction_receive(Phone, Bid, EndDate - erlang:system_time(second), EndDate);
%%
%%    %% Receive TIMER message from a Bidder
%%        {timer, PhoneName} ->
%%            RemainingTime = get_time_remaining(EndDate),
%%            CurrentWinner = erws_mnesia:get_winner_bidder(PhoneName),
%%            case CurrentWinner of
%%                {WinnerEmail, _} ->
%%                    gproc:send({p, l, {?MODULE, PhoneName}}, {send_timer, Bid, RemainingTime, WinnerEmail});
%%                not_found ->
%%                    gproc:send({p, l, {?MODULE, PhoneName}}, {send_timer, Bid, RemainingTime, []})
%%            end,
%%            auction_receive(Phone, Bid, EndDate - erlang:system_time(second), EndDate);
%%
%%    %% Receive BID message from a Bidder
%%        {send, BidderEmail, NewBid, HandlerPid, PhoneName} ->
%%            logger:info("[auction_receive] => email: ~p, NewBid: ~p, Bid: ~p, From: ~p ~n", [BidderEmail, NewBid, Bid, HandlerPid]),
%%            RemainingTime = get_time_remaining(EndDate),
%%            if
%%                Bid < NewBid ->
%%                    logger:info("[auction_receive] => NewBid: ~p > Current Bid: ~p!~n", [NewBid, Bid]),
%%                    logger:info("[auction_receive] => Send update bid to bidders"),
%%                    gproc:send({p, l, {?MODULE, PhoneName}}, {new_bid, NewBid, RemainingTime, BidderEmail}),
%%                    erws_mnesia:save_bid(PhoneName, BidderEmail, NewBid),
%%                    auction_receive(Phone, NewBid, EndDate - erlang:system_time(second), EndDate);
%%                true ->
%%                    gproc:send({p, l, {?MODULE, PhoneName}}, {no_bid, Bid, RemainingTime}),
%%                    auction_receive(Phone, Bid, EndDate - erlang:system_time(second), EndDate)
%%            end
%%    after AuctionTime * 1000 -> % Convert DelayInSeconds to milliseconds
%%        case erws_mnesia:get_winner_bidder(Phone) of
%%            not_found ->
%%                logger:info("[auction_receive] => No bidders for the auction of the phone: ~p~n", [Phone]),
%%                RemainingTime = get_time_remaining(EndDate),
%%                Response = <<"No bidders">>,
%%                gproc:send({p, l, {?MODULE, Phone}}, {no_bidders, Response, RemainingTime});
%%            {WinnerEmail, WinningBid} ->
%%                RemainingTime = get_time_remaining(EndDate),
%%                logger:info("[auction_receive] => Phone: ~p, Winner: ~p, Winning Bid: ~p", [Phone, WinnerEmail, WinningBid]),
%%                gproc:send({p, l, {?MODULE, Phone}}, {winner_bidder, Phone, WinnerEmail, WinningBid, RemainingTime});
%%            _ ->
%%                logger:error("[auction_receive] => Unexpected result from get_winner_bidder")
%%        end
%%    end.

% Handles incoming WebSocket text frames
websocket_handle(Frame = {text, Message}, State) ->
    logger:info("[websocket_handle] => Frame: ~p, State: ~p~n", [Frame, State]),
    logger:info("[websocket_handle] => Received ~p~n", [Frame]),

    DecodedMessage = jsone:try_decode(Message),

    Response = case element(1, DecodedMessage) of
                   ok ->
                       Json = element(2, DecodedMessage),
                       logger:info("[websocket_handle] => Decoded ~p~n", [Json]),
                       handle_websocket_frame(Json, State);
                   error ->
                       logger:info("[websocket_handle] => Failed to decode JSON: ~p~n", [Message]),
                       {ok, State}
               end,
    Response.

% Handle a frame after JSON decoding
handle_websocket_frame(Map, State) ->
%%  logger:debug("[erws_handler] handle_websocket_frame => Map is ~p~n", [Map]),
    Action = maps:get(<<"action">>, Map),
    logger:debug("[handle_websocket_frame] => Action: ~p~n", [Action]),

    case Action of
        <<"new_auction">> -> % Handle new auction action
            handle_new_auction(Map);

        <<"join_auction">> ->
            BidderEmail = maps:get(<<"email">>, Map),
            logger:debug("[handle_websocket_frame] => EMAIL of the new join user: ~p~n", [BidderEmail]),
            handle_join_auction(Map, State);

        <<"live_auctions">> ->
            handle_live_auctions(State);

        <<"send">> ->
            handle_send_bid(Map, State);

        <<"timer">> ->
            handle_get_timer(Map, State);

        _ ->
            logger:info("[handle_websocket_frame] => Unknown action: ~p~n", [Action]),
            {ok, State}
    end.


handle_new_auction(Map) ->
    StartDate = maps:get(<<"startSeconds">>, Map),
    EndDate = maps:get(<<"endSeconds">>, Map),
    case is_integer(StartDate) andalso is_integer(EndDate) of
        true ->
            logger:info("[handle_new_auction] => New auction scheduled for ~p~n", [StartDate]),
            CurrentDate = erlang:system_time(second),
            Delay = StartDate - CurrentDate,
            AuctionTime = EndDate - StartDate,
            case Delay > 0 of
                true ->
                    timer:sleep(Delay * 1000),
                    PhoneName = maps:get(<<"phoneName">>, Map),
                    MinimumPrice = maps:get(<<"minimumPrice">>, Map),
                    erws_dynamic_sup:start_auction_process(PhoneName, MinimumPrice, AuctionTime, EndDate);
%%                    erws_sup:start_auction_process(PhoneName, MinimumPrice, AuctionTime, EndDate),
%%                    erws_dynamic_sup:start_auction_process(PhoneName, MinimumPrice, AuctionTime, EndDate);

%%                    AuctionPid = spawn(fun() -> auction_handle(PhoneName, MinimumPrice, AuctionTime, EndDate) end),
%%                    erws_mnesia:save_auction(PhoneName, AuctionPid),
%%                    Text = "Live auctions update requested!",
%%                    gproc:send({p, l, {?MODULE, {live_auctions}}}, {live_auctions_update, Text}),
%%                    AuctionPid = erws_mnesia:get_auction_pid(PhoneName),
%%                    logger:info("[handle_new_auction] => Auction process spawned with pid: ~p~n", [AuctionPid]),
%%                    {ok, AuctionPid}; % Return the tuple {ok, AuctionPid}
                false ->
                    logger:info("[handle_new_auction] => Start date has already passed, cannot spawn auction process.~n"),
                    undefined % Return undefined or any other value to indicate failure
            end;
        false ->
            logger:info("[handle_new_auction] => Invalid start date~n"),
            undefined % Return undefined or any other value to indicate failure
    end.

handle_join_auction(Map, State) ->
    PhoneName = maps:get(<<"phoneName">>, Map),
    AuctionPid = erws_mnesia:get_auction_pid(PhoneName),
    logger:info("[handle_join_auction] => The phone ~p has the following PID: ~p~n", [PhoneName, AuctionPid]),
    logger:info("[handle_join_auction] => Starting to spawning a bidder for the auction with pid: ~p~n", [AuctionPid]),

    erws_bidder_handler:start(AuctionPid, PhoneName),
    gproc:reg({p, l, {?MODULE, PhoneName}}),
    Pid = gproc:lookup_pids({p, l, {?MODULE, PhoneName}}),
    logger:info("[handle_join_auction] => WEBSOCKET_INIT process started here: ~p~n", [Pid]),
    receive_joined(State).

handle_live_auctions(State) ->
    gproc:reg({p, l, {?MODULE, {live_auctions}}}),
    logger:info("[handle_live_auctions] => Client registered to the live auctions session!"),
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
    logger:info("[handle_send_bid] => Successfully received bid from client: ~p~n, ~p~n, ~p~n, ~p~n", [PhoneName, BidderEmail, BidDate, BidValue]),

    AuctionPid = erws_mnesia:get_auction_pid(PhoneName),
    logger:info("[handle_send_bid] => Retrieved AuctionPid saved in AUCTION table of MNESIA DB: ~p~n", [AuctionPid]),

    erws_bidder_handler:process_bid(AuctionPid, BidderEmail, BidValue, self(), PhoneName),
    receive
        {new_bid, NewBid, RemainingTime, CurrentWinner} ->
            logger:info("[handle_send_bid] => Received New Bid"),
            Response = io_lib:format("Bid:~p RemainingTime:~p CurrentWin:~p", [NewBid, RemainingTime, CurrentWinner]),
            {reply, {text, Response}, State, hibernate};
        {no_bid, Bid} ->
            logger:info("[handle_send_bid] => Received Bid < Current Max Bid"),
            Response = io_lib:format("Bid:~p", [Bid]),
            {reply, {text, Response}, State, hibernate};
        {winner_bidder, Phone, WinnerEmail, WinningBid, RemainingTime} ->
            logger:info("[handle_send_bid] => Phone: ~p, Winner: ~p, Winning Bid: ~p", [Phone, WinnerEmail, WinningBid]),
            Response = io_lib:format("Phone:~p Winner:~p Winning Bid:~p RemainingTime:~p", [Phone, WinnerEmail, WinningBid, RemainingTime]),
            {reply, {text, Response}, State, hibernate}
    end.


handle_get_timer(Map, State) ->
    PhoneName = maps:get(<<"phone_name">>, Map),

    AuctionPid = erws_mnesia:get_auction_pid(PhoneName),
    logger:info("[handle_get_timer] => Retrieved AuctionPid saved in AUCTION table of MNESIA DB: ~p~n", [AuctionPid]),

    erws_bidder_handler:process_timer(AuctionPid, PhoneName),

    receive
        {send_timer, Bid, RemainingTime, CurrentWinner} ->
            logger:info("[handle_get_timer] => Get Auction Timer"),
            Response = io_lib:format("Bid:~p RemainingTime:~p CurrentWin:~p", [Bid, RemainingTime, CurrentWinner]),
            {reply, {text, Response}, State, hibernate}
    end.


%%% Function to compute the auction remaining time
%%get_time_remaining(EndDate) ->
%%    % Compute the remaining time in seconds
%%    RemainingTimeSeconds = EndDate - erlang:system_time(second),
%%    % Convert the remaining time in a readable format
%%    RemainingDays = RemainingTimeSeconds div 86400,
%%    RemainingHours = (RemainingTimeSeconds rem 86400) div 3600,
%%    RemainingMinutes = ((RemainingTimeSeconds rem 86400) rem 3600) div 60,
%%    RemainingSeconds = (RemainingTimeSeconds rem 60),
%%    % Format the remaining time in a readable string
%%    TimeRemainingString = io_lib:format("~b d ~b h ~b m ~b s", [RemainingDays, RemainingHours, RemainingMinutes, RemainingSeconds]),
%%    TimeRemainingString.


receive_joined(State) ->
    receive
        {joined, Bid, RemainingTime, CurrentWinner} ->
            Response = io_lib:format("Bid:~p RemainingTime:~p CurrentWin:~p", [Bid, RemainingTime, CurrentWinner]),
            {reply, {text, Response}, State, hibernate};
        {not_joined} ->
            logger:info("[receive_joined] => Bidder not joined!"),
            {ok, State};
        {new_bid, NewBid, RemainingTime, CurrentWinner} ->
            logger:info("[receive_joined] => Received New Bid"),
            Response = io_lib:format("Bid:~p RemainingTime:~p CurrentWin:~p", [NewBid, RemainingTime, CurrentWinner]),
            {reply, {text, Response}, State, hibernate};
        {no_bid, Bid} ->
            logger:info("[receive_joined] => Received Bid < Current Max Bid"),
            Response = io_lib:format("Bid:~p", [Bid]),
            {reply, {text, Response}, State, hibernate};
        {no_bidders, Text, RemainingTime} ->
            logger:info("[receive_joined] => Auction terminated, no bidders"),
            Response = io_lib:format("Winner:~p RemainingTime:~p", [Text, RemainingTime]),
            {reply, {text, Response}, State, hibernate};
        {winner_bidder, Phone, WinnerEmail, WinningBid, RemainingTime} ->
            logger:info("[receive_joined] => Phone: ~p, Winner: ~p, Winning Bid: ~p", [Phone, WinnerEmail, WinningBid]),
            Response = io_lib:format("Phone:~p Winner:~p Winning Bid:~p RemainingTime:~p", [Phone, WinnerEmail, WinningBid, RemainingTime]),
            {reply, {text, Response}, State, hibernate};
        {send_timer, Bid, RemainingTime, CurrentWinner} ->
            logger:info("[receive_joined] => Get Auction Timer"),
            Response = io_lib:format("Bid:~p RemainingTime:~p CurrentWin:~p", [Bid, RemainingTime, CurrentWinner]),
            {reply, {text, Response}, State, hibernate}
    end.

% Terminate WebSocket connection
websocket_terminate(_Reason, _Req, _State) ->
    ok.

% Terminate function
terminate(_Reason, _Req, _State) ->
    ok.


websocket_info(Info, State) ->
    case Info of
        {joined, Bid, RemainingTime, CurrentWinner} ->
            Response = io_lib:format("[websocket_info] => Bid:~p RemainingTime:~p CurrentWin:~p", [Bid, RemainingTime, CurrentWinner]),
            {reply, {text, Response}, State, hibernate};
        {not_joined} ->
            logger:info("[websocket_info] => Bidder not joined!"),
            {ok, State};
        {new_bid, NewBid, RemainingTime, CurrentWinner} ->
            logger:info("[websocket_info] => Received New Bid"),
            Response = io_lib:format("Bid:~p RemainingTime:~p CurrentWin:~p", [NewBid, RemainingTime, CurrentWinner]),
            {reply, {text, Response}, State, hibernate};
        {no_bid, Bid} ->
            logger:info("[websocket_info] => Received Bid < Current Max Bid"),
            Response = io_lib:format("Bid:~p", [Bid]),
            {reply, {text, Response}, State, hibernate};
        {no_bidders, Text, RemainingTime} ->
            logger:info("[websocket_info] => Auction terminated, no bidders"),
            Response = io_lib:format("Winner:~p RemainingTime:~p", [Text, RemainingTime]),
            {reply, {text, Response}, State, hibernate};
        {winner_bidder, Phone, WinnerEmail, WinningBid, RemainingTime} ->
            logger:info("[websocket_info] => Phone: ~p, Winner: ~p, Winning Bid: ~p", [Phone, WinnerEmail, WinningBid]),
            Response = io_lib:format("Phone:~p Winner:~p Winning Bid:~p RemainingTime:~p", [Phone, WinnerEmail, WinningBid, RemainingTime]),
            {reply, {text, Response}, State, hibernate};
        {send_timer, Bid, RemainingTime, CurrentWinner} ->
            logger:info("[websocket_info] => Get Auction Timer"),
            Response = io_lib:format("Bid:~p RemainingTime:~p CurrentWin:~p", [Bid, RemainingTime, CurrentWinner]),
            {reply, {text, Response}, State, hibernate};
        {live_auctions_update, Text} ->
            Response = io_lib:format("~p", [Text]),
            {reply, {text, Response}, State, hibernate}
    end.
