%%%-------------------------------------------------------------------
%%% @author debian
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Apr 2024 12:56 PM
%%%-------------------------------------------------------------------
-module(erws_bidder_handler).

%% Exported Functions
-export([start/3,init_bidder/3,process_requests/0,process_bid/6]).

%% API Functions
%% Create a bidder process
start(AuctionPid, BidderName, HandlerPid) ->
  BidderPid = spawn(erws_bidder_handler, init_bidder, [AuctionPid, BidderName, HandlerPid]).
%%  process_commands(AuctionPid, BidderName, BidderPid).

%% Initialize bidder process - send to agent bidder's information
init_bidder(AgentPid, BidderName, HandlerPid) ->
  AgentPid ! {bidder_join, BidderName, self(), HandlerPid},
  process_requests().

%% Local Functions
process_requests() ->
  receive
    {join, Name, Bid} ->
      logger:info("[JOIN] ~s joins the auction. Current Bid: ~p ~n", [Name,Bid]),
      process_requests();
    {message, Name, Text} ->
      logger:info("[BID] New Bid of ~p from ~p~n", [Text, Name]),
      process_requests()
  end.

% Function to process the bid received from an user
process_bid(AuctionPid, BidderEmail, BidderPid, BidValue, HandlerPid, State) ->
    AuctionPid ! {send, BidderEmail, BidValue, BidderPid, HandlerPid, State}.



