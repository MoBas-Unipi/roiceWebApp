-module(erws_bidder_handler).

-export([start/2, init_bidder/2, process_bid/5, process_timer/2]).

%% Create a bidder process
start(AuctionPid, PhoneName) ->
    spawn(erws_bidder_handler, init_bidder, [AuctionPid, PhoneName]).
%%    spawn(fun() -> erws_sup:start_bidder_process(AuctionPid, PhoneName) end).


%% Initialize bidder process - send to the auction agent bidder's information
init_bidder(AgentPid, PhoneName) ->
    AgentPid ! {bidder_join, PhoneName}.

% Function to process the bid received from an user
process_bid(AuctionPid, BidderEmail, BidValue, HandlerPid, PhoneName) ->
    AuctionPid ! {send, BidderEmail, BidValue, HandlerPid, PhoneName}.

% Function to process the get timer request from an user
process_timer(AuctionPid, PhoneName) ->
    AuctionPid ! {timer, PhoneName}.

