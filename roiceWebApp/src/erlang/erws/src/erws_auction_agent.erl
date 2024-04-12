-module(erws_auction_agent).

-export([
  init/2,
  handle/2,
  terminate/3,
  auction_handle/4,
  websocket_handle/2,
  websocket_info/3,
  websocket_terminate/3
]).

-define(initVal, 200).

% Initialize WebSocket connection
init(Req, State) ->
  {cowboy_websocket, Req, State}.



% Handle HTTP requestsagent
handle(Req, State) ->
  logger:debug("[erws_auction_agent] - handle => Request not expected: ~p", [Req]),
  {ok, Req2} = cowboy_http_req:reply(404, [{'Content-Type', <<"text/html">>}]),
  {ok, Req2, State}.

% Handle auction messages
auction_handle(Bidders, Bid, AuctionTime, EndDate) ->
  receive
  %% Receive JOIN request from a Bidder
    {bidder_join, Name, From} ->
      % Add Bidder into the list and broadcast the new list
      NewBidders = [{From, Name, 0} | Bidders],
      broadcast(NewBidders, {join, Name, Bid}),
      auction_handle(NewBidders, Bid, EndDate - erlang:system_time(second), EndDate);

  %% Receive LEAVE request from a Bidder
    {bidder_leave, Name, From} ->
      logger:info("Bidder ~w LEAVES.~n", [Name]),
      % Delete Bidder from the list and broadcast the new list to rest of bidders
      NewBidders = lists:keydelete(From, 1, Bidders),
      if
      %% If new list is empty, send a message to the node leaving
        NewBidders == [] ->
          Bid2 = ?initVal,
          broadcast(Bidders, {leave, Name, Bid2});
        true ->
          %% If new list not empty, change its structure in order to find the max bid in the list
          ModBidders = changeStrc(NewBidders, []),
          %% Bid2 has the max bid of the list of peers
          {Bid2, [User, _]} = lists:max(ModBidders),
          if
          %% If Bid2 is less than initial value, it broadcasts the initial value
            Bid2 < ?initVal ->
              broadcast(Bidders, {leave, Name, ?initVal});
            true ->
              %% Else, it broadcasts the Bid2 and the bidder doing this bid
              broadcast(Bidders, {leave, Name, Bid2, User})
          end
      end,
      auction_handle(NewBidders, Bid2, EndDate - erlang:system_time(second), EndDate);

  %% Receive BID message from a Bidder
    {send, Name, NewBid, From} ->
      % If received bid is higher than existing one, delete old bid of this Bidder
      % and broadcast new bid of Bidder
      logger:info("name: ~w, NewBid: ~w, Bid: ~w, From: ~w ~n", [Name, NewBid, Bid, From]),
      if
        Bid < NewBid ->
          NewBidders = lists:keydelete(Name, 2, Bidders),
          logger:info("[~s] ~p ~n", ["NewBid > Bid. Delete bidder:", From]),
          broadcast([{From, Name, NewBid} | NewBidders], {message, Name, NewBid}),
          auction_handle([{From, Name, NewBid} | NewBidders], NewBid, EndDate - erlang:system_time(second), EndDate);
        true ->
          %% Else IGNORE it!
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
  Fun = fun({Peer, _, _}) -> Peer ! Message end,
  lists:map(Fun, PeerList).

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
  logger:info("[erws_handler] handle_websocket_frame => Map is ~p~n", [Map]),
  Action = maps:get(<<"action">>, Map),
  logger:info("[erws_handler] handle_websocket_frame => Action: ~p~n", [Action]),

  case Action of
    <<"new_auction">> -> % Handle new auction action
      {ok, AuctionPid} = handle_new_auction(Map, State);
    <<"join_auction">> -> % Handle join auction action

      {ok, AuctionPid} = handle_join_auction(Map, State);
    <<"send">> ->
        handle_send_bid(Map,State);

    _ ->
      logger:info("[erws_handler] handle_websocket_frame => Unknown action: ~p~n", [Action]),
      {ok, State}
  end.


handle_new_auction(Map, State) ->
  logger:info("[erws_handler] handle_new_auction => Map is ~p~n", [Map]),
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
          AuctionPid = spawn(fun() -> auction_handle(State, ?initVal, AuctionTime, EndDate) end),
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
  BidderPid = erws_bidder_handler:start(AuctionPid, BidderEmail),
  % Save BidderEmail and BidderPid in bidder table of MNESIA DB
  erws_mnesia:save_bidder(BidderEmail,BidderPid),
  % Get the newly inserted bidder from the BIDDER table
  NewBidderPid = erws_mnesia:get_bidder_pid(BidderEmail),
  logger:info("Bidder record saved in BIDDER table of MNESIA DB: ~p~n", [NewBidderPid]),
  {ok, State}.


handle_send_bid(Map, State) ->
  % Get attributes from map
  PhoneName = maps:get(<<"phone_name">>, Map),
  BidderEmail = maps:get(<<"email">>, Map),
  BidDate = maps:get(<<"date">>, Map),
  BidValue = maps:get(<<"value">>, Map),
  logger:info("Successfully received bid from client: ~p~n, ~p~n, ~p~n, ~p~n",[PhoneName,BidderEmail,BidDate,BidValue]),

  % TODO Get the AuctionPid from auction table in the DB
  AuctionPid = erws_mnesia:get_auction_pid(PhoneName),
  logger:info("Retrieved AuctionPid saved in AUCTION table of MNESIA DB: ~p~n", [AuctionPid]),

  % TODO Get the BidderPid from bidder table in the DB
  BidderPid = erws_mnesia:get_bidder_pid(BidderEmail),
  logger:info("Retrieved BidderPid saved in BIDDER table of MNESIA DB: ~p~n", [BidderPid]),

  % TODO call the erws_bidder_handler function to send the bid
  erws_bidder_handler:process_bid(AuctionPid, BidderEmail, BidderPid, BidValue),

  % Save bid to the bid table (TODO: IF THE NEW BID IS HIGHER)
  %erws_mnesia:save_bid(PhoneName, BidderEmail, BidDate, BidValue),

  % Get the newly inserted bid record from the bid table
  %NewBidRecord = erws_mnesia:get_bid(PhoneName),
  %logger:info("Bid record saved in BID table of MNESIA DB: ~p~n", [NewBidRecord]),


  % Display saved bids in the DB
  %erws_mnesia:print_bids(),
  {ok, State}.



% Handle WebSocket timeout messages
websocket_info({timeout, _Ref, Msg}, Req, State) ->
  {reply, {text, Msg}, Req, State};

% Handle other WebSocket info messages
websocket_info(_Info, Req, State) ->
  logger:debug("[erws_handler] websocket_info => websocket info"),
  {ok, Req, State, hibernate}.

% Terminate WebSocket connection
websocket_terminate(_Reason, _Req, _State) ->
  ok.

% Terminate function
terminate(_Reason, _Req, _State) ->
  ok.
