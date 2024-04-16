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
-export([start/4,init_bidder/4,process_requests/0,process_bid/5]).

%% API Functions
%% Create a bidder process
start(AuctionPid, BidderEmail, HandlerPid, PhoneName) ->
  ProcessPid = spawn(erws_bidder_handler, init_bidder, [AuctionPid, BidderEmail, HandlerPid, PhoneName]).
%%  process_commands(AuctionPid, BidderName, BidderPid).

%% Initialize bidder process - send to agent bidder's information
init_bidder(AgentPid, BidderEmail, HandlerPid, PhoneName) ->
  AgentPid ! {bidder_join, BidderEmail, HandlerPid, PhoneName},
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
process_bid(AuctionPid, BidderEmail, BidValue, HandlerPid, PhoneName) ->
    AuctionPid ! {send, BidderEmail, BidValue, HandlerPid, PhoneName}.



