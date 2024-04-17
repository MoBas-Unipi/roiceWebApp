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
-export([start/2,init_bidder/2,process_bid/5]).

%% API Functions
%% Create a bidder process
start(AuctionPid, PhoneName) ->
  spawn(erws_bidder_handler, init_bidder, [AuctionPid, PhoneName]).
%%  process_commands(AuctionPid, BidderName, BidderPid).

%% Initialize bidder process - send to agent bidder's information
init_bidder(AgentPid, PhoneName) ->
  AgentPid ! {bidder_join, PhoneName}.

% Function to process the bid received from an user
process_bid(AuctionPid, BidderEmail, BidValue, HandlerPid, PhoneName) ->
    AuctionPid ! {send, BidderEmail, BidValue, HandlerPid, PhoneName}.



