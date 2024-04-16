-module(erws_auction_agent).

-export([
  init/2,
  websocket_init/3,
  handle/2,
  terminate/3,
  auction_handle/4,
  receive_joined/1,
  websocket_handle/2,
  websocket_info/2,
  websocket_info/3,
  websocket_terminate/3
]).

% Initialize WebSocket connection
init(Req, State) ->
  {cowboy_websocket, Req, State}.

websocket_init(_TransportName, Req, _Opts) ->
  {ok, Req, _TransportName}.

% Handle HTTP requestsagent
handle(Req, State) ->
  logger:debug("[erws_auction_agent] - handle => Request not expected: ~p", [Req]),
  {ok, Req2} = cowboy_http_req:reply(404, [{'Content-Type', <<"text/html">>}]),
  {ok, Req2, State}.

% Handle auction messages
auction_handle(Bidders, Bid, AuctionTime, EndDate) ->
  receive
  %% Receive JOIN request from a Bidder
    {bidder_join, BidderEmail, HandlerPid, PhoneName} ->
%%      erws_mnesia:add_bidder_to_auction(PhoneName,HandlerPid),
      AuctionBidders = erws_mnesia:get_auction_bidders(PhoneName),
      AuctionPid = erws_mnesia:get_auction_pid(PhoneName),
      logger:info("Bidders joined for the Auction with PID ~p: ~p~n", [AuctionPid,AuctionBidders]),
      % Add Bidder into the list and broadcast the new list
      AuctionBiddersUpdated = erws_mnesia:add_bidder_to_auction(PhoneName, {HandlerPid, BidderEmail, 0}),
      logger:info("New Bidders for the Auction with PID ~p: ~p~n", [AuctionPid,AuctionBiddersUpdated]),
      Key = {?MODULE,auction_agent_process},
      gproc:send({p,l,Key},{joined,Bid}),
      auction_handle(AuctionBiddersUpdated, Bid, EndDate - erlang:system_time(second), EndDate);

  %% Receive BID message from a Bidder
    {send, BidderEmail, NewBid, HandlerPid, PhoneName} ->
      % If received bid is higher than existing one, delete old bid of this Bidder
      % and broadcast new bid of Bidder
      logger:info("email: ~w, NewBid: ~w, Bid: ~w, From: ~w ~n", [BidderEmail, NewBid, Bid, HandlerPid]),
      AuctionBidders = erws_mnesia:get_auction_bidders(PhoneName),
      logger:info("Bidders joined to the Auction: ~p~n", [AuctionBidders]),
      if
        Bid < NewBid ->
          logger:info("[~s] ~p ~n", ["NewBid > Bid. Delete bidder:", HandlerPid]),
          erws_mnesia:delete_bidder(HandlerPid, PhoneName),
          UpdateBidders = erws_mnesia:add_bidder_to_auction(PhoneName, {HandlerPid, BidderEmail, NewBid}),
          logger:info("Send update bid to bidders"),
          Key = {?MODULE,auction_agent_process},
          gproc:send({p,l,Key},{new_bid,NewBid}),
%%          AuctionBidders = erws_mnesia:get_auction_bidders(PhoneName),
%%          broadcast_to_clients(NewBid, AuctionBidders),
%%          HandlerPid ! {new_bid, NewBid},
          auction_handle(UpdateBidders, NewBid, EndDate - erlang:system_time(second), EndDate);
        true ->
          broadcast(AuctionBidders, {no_bid}),
          auction_handle(Bidders, Bid, EndDate - erlang:system_time(second), EndDate)
      end
  after AuctionTime * 1000 -> % Convert DelayInSeconds to milliseconds
    logger:info("Auction timeout reached after ~p seconds~n", [AuctionTime]),
    %% Your timeout logic here
    case Bidders of
      [] ->
        %% If there are no bidders
        logger:info("Timeout! No bidders! Auction terminated!~n");
      _ ->
        %% Else, if there are bidders but no high bids
        ModBidders = changeStrc(Bidders, []),
        case lists:max(ModBidders) of
          {Bid2, [User, _]} when Bid2 > 0 ->
            logger:info("Sold to ~s! Final price: ~p~n", [User, Bid2]);
          _ ->
            logger:info("Timeout! No high bids! Auction terminated~n")
        end
    end
  end.

% Local Functions
% broadcast - send message to list of peers
broadcast(PeerList, Message) ->
  logger:info("PeerList: ~p", [PeerList]),
  Fun = fun({Peer, _, _}) -> Peer ! Message end,
  lists:map(Fun, PeerList).

%%broadcast_to_clients(_, []) ->
%%  ok;
%%broadcast_to_clients(Msg, [Client|Rest]) ->
%%  Client ! {new_bid, Msg},
%%  broadcast_to_clients(Msg,Rest).

% changeStrc - change structure of list with bidders
changeStrc([{Peer, Name, Value} | T], Tail) ->
  [{Value, [Name, Peer]} | changeStrc(T, Tail)];
changeStrc([], Tail) ->    % End of recursion
  Tail.

% Handles incoming WebSocket text frames
websocket_handle(Frame = {text, Message}, State) ->
  logger:info("[erws_handler] websocket_handle => Frame: ~p, State: ~p~n", [Frame, State]),
  logger:info("[erws_handler] websocket_handle => Received ~p~n", [Frame]),

  DecodedMessage = jsone:try_decode(Message),

  Response = case element(1, DecodedMessage) of
               ok ->
                 Json = element(2, DecodedMessage),
                 logger:info("[erws_handler] websocket_handle => Decoded ~p~n", [Json]),
                 handle_websocket_frame(Json, State);
               error ->
                 logger:info("[erws_handler] websocket_handle => Failed to decode JSON: ~p~n", [Message]),
                 {ok, State}
             end,
  Response;

% Handle other WebSocket messages (if necessary)
websocket_handle(_Any, State) ->
  {reply, {text, <<"whut?">>}, State, hibernate}.

% Handle a frame after JSON decoding
handle_websocket_frame(Map, State) ->
%%  logger:debug("[erws_handler] handle_websocket_frame => Map is ~p~n", [Map]),
  Action = maps:get(<<"action">>, Map),
  logger:debug("[erws_handler] handle_websocket_frame => Action: ~p~n", [Action]),

  case Action of
    <<"new_auction">> -> % Handle new auction action
      handle_new_auction(Map, State);
    <<"join_auction">> ->
      BidderEmail = maps:get(<<"email">>, Map),
      PhoneName = maps:get(<<"phoneName">>, Map),
      AuctionPid = erws_mnesia:get_auction_pid(PhoneName),
      logger:debug("EMAIL of the new join user: ~p~n", [BidderEmail]),
      Bool = erws_mnesia:is_bidder_present(AuctionPid,BidderEmail),
      logger:debug("Bool is present: ~p~n", [Bool]),
%%        true ->
%%          logger:info("[erws_handler] handle_websocket_frame => Bidder already joined the auction: p~n", [AuctionPid]),
%%          receive_joined(State);
%%        false ->
          handle_join_auction(Map, State);
%%      end;
    <<"send">> ->
      handle_send_bid(Map,State);
    _ ->
      logger:info("[erws_handler] handle_websocket_frame => Unknown action: ~p~n", [Action]),
      {ok, State}
  end.


handle_new_auction(Map, State) ->
%%  logger:debug("[erws_handler] handle_new_auction => Map is ~p~n", [Map]),
  StartDate = maps:get(<<"startSeconds">>, Map),
  EndDate = maps:get(<<"endSeconds">>, Map),
  case is_integer(StartDate) andalso is_integer(EndDate) of
    true ->
      logger:info("[erws_handler] handle_new_auction => New auction scheduled for ~p~n", [StartDate]),
      CurrentDate = erlang:system_time(second),
      Delay = StartDate - CurrentDate,
      AuctionTime = EndDate - StartDate,
      case Delay > 0 of
        true ->
          timer:sleep(Delay * 1000),
          MinimumPrice = maps:get(<<"minimumPrice">>, Map),
          AuctionPid = spawn(fun() -> auction_handle(State, MinimumPrice, AuctionTime, EndDate) end),
          logger:info("Auction process spawned with pid: ~p~n", [AuctionPid]),
          PhoneName = maps:get(<<"phoneName">>, Map),
          erws_mnesia:save_auction(PhoneName, AuctionPid),
          erws_mnesia:print_auctions(),
          {ok, AuctionPid}; % Return the tuple {ok, AuctionPid}
        false ->
          logger:info("Start date has already passed, cannot spawn auction process.~n"),
          undefined % Return undefined or any other value to indicate failure
      end;
    false ->
      logger:info("[erws_handler] handle_new_auction => Invalid start date~n"),
      undefined % Return undefined or any other value to indicate failure
  end.

handle_join_auction(Map, State) ->
      BidderEmail = maps:get(<<"email">>, Map),
      PhoneName = maps:get(<<"phoneName">>, Map),
      AuctionPid = erws_mnesia:get_auction_pid(PhoneName),
      logger:info("[erws_handler] handle_join_auction => The phone ~p has the following PID: ~p~n", [PhoneName, AuctionPid]),
      logger:info("[erws_handler] handle_join_auction => Starting to spawning a bidder for the auction with pid: ~p~n", [AuctionPid]),

      % Return BidderPid
      erws_bidder_handler:start(AuctionPid, BidderEmail, self(), PhoneName),
      % Save BidderEmail and BidderPid in bidder table of MNESIA DB
%%      erws_mnesia:save_bidder(BidderEmail,BidderPid),
      % Get the newly inserted bidder from the BIDDER table
%%  NewBidderPid = erws_mnesia:get_bidder_pid(BidderEmail),
%%  logger:info("Bidder record saved in BIDDER table of MNESIA DB: ~p~n", [NewBidderPid]),
      gproc:reg({p,l,{?MODULE,auction_agent_process}}),
      Key = {?MODULE,auction_agent_process},
      Pid = gproc:lookup_pids({p,l,Key}),
      logger:info("WEBSOCKET_INIT process started here: ~p~n", [Pid]),
      receive_joined(State).


handle_send_bid(Map, State) ->
  % Get attributes from map
  PhoneName = maps:get(<<"phone_name">>, Map),
  BidderEmail = maps:get(<<"email">>, Map),
  BidDate = maps:get(<<"date">>, Map),
  BidValue = maps:get(<<"value">>, Map),
  logger:info("Successfully received bid from client: ~p~n, ~p~n, ~p~n, ~p~n",[PhoneName,BidderEmail,BidDate,BidValue]),

  AuctionPid = erws_mnesia:get_auction_pid(PhoneName),
  logger:info("Retrieved AuctionPid saved in AUCTION table of MNESIA DB: ~p~n", [AuctionPid]),

  erws_bidder_handler:process_bid(AuctionPid, BidderEmail, BidValue, self(), PhoneName),
  receive
    {new_bid, NewBid} ->
      logger:info("[handle_send_bid] => Received New Bid"),
      Response = io_lib:format("~p", [NewBid]),
      {reply, {text, Response}, State, hibernate};
    {no_bid} ->
      logger:info("[handle_send_bid] => Received Bid < Current Max Bid"),
      {ok, State}
  end.

receive_joined(State) ->
  receive
    {joined, Bid} ->
      Response = io_lib:format("~p", [Bid]),
      {reply, {text, Response}, State, hibernate};
    {not_joined} ->
      logger:info("[handle_join_auction] => Bidder not joined!"),
      {ok, State};
    {new_bid, NewBid} ->
      logger:info("[receive_joined] => Received New Bid"),
      Response = io_lib:format("~p", [NewBid]),
      {reply, {text, Response}, State, hibernate}
  end.

% Handle WebSocket timeout messages
%%websocket_info({text, NewBid}, State) ->
%%  receive
%%    {new_bid, NewBid} ->
%%      logger:info("[receive_joined] => Received New Bid"),
%%      Response = io_lib:format("~p", [NewBid]),
%%      {reply, {text, Response}, State, hibernate}
%%  end.

% Handle other WebSocket info messages
%%websocket_info(_Info, Req, State) ->
%%  logger:debug("[erws_handler] websocket_info => websocket info"),
%%  {ok, Req, State, hibernate}.

% Terminate WebSocket connection
websocket_terminate(_Reason, _Req, _State) ->
  ok.

% Terminate function
terminate(_Reason, _Req, _State) ->
  ok.

websocket_info(Msg, Req, State) ->
  io:format("[chatroom_websocket] websocket_info({send_message, Msg}, State) => Send message ~p~n", [Msg]),
  {reply,{text, Msg}, Req, State}.

websocket_info(Info, State) ->
  case Info of
    {joined, Bid} ->
      Response = io_lib:format("~p", [Bid]),
      {reply, {text, Response}, State, hibernate};
    {not_joined} ->
      logger:info("[handle_join_auction] => Bidder not joined!"),
      {ok, State};
    {new_bid, NewBid} ->
      logger:info("[receive_joined] => Received New Bid"),
      Response = io_lib:format("~p", [NewBid]),
      {reply, {text, Response}, State, hibernate}
  end.
