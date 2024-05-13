%%%-------------------------------------------------------------------
%%% @doc
%%%   This module is responsible for setting up Mnesia on the cluster nodes.
%%%   It creates the schema and creates auction and bid tables on the remote nodes.
%%% @end
%%%-------------------------------------------------------------------
-module(mnesia_setup).

%% API
-export([
    init/1,
    create_auction_table/1,
    create_bid_table/1
]).

-record(auction, {phone_name, auction_pid}).
-record(bid, {phone_name, current_winner_user_email, bid_value}).

% Function to set up Mnesia tables.
init(Nodes) ->
    logger:info("[mnesia_setup] => Init Mnesia"),
    % Create Mnesia schema and start Mnesia.
    mnesia:create_schema([node()] ++ Nodes),
    mnesia:start(),
    % Create auction and bid tables.
    create_auction_table(Nodes),
    create_bid_table(Nodes),
    % Print info about Mnesia.
    timer:sleep(2000),
    mnesia:info().

% Function to create the auction table if it doesn't exist.
create_auction_table(Nodes) ->
    case mnesia:create_table(auction, [
        {attributes, record_info(fields, auction)},
        {disc_copies, Nodes}
    ]) of
        {atomic, ok} ->
            logger:info("[mnesia_setup] create_auction_table => Auction table created!~n"),
            ok;
        {aborted, Reason} ->
            logger:error("[mnesia_setup] create_auction_table => Auction table not created: ~p~n", [Reason])
    end.

% Function to create the bid table if it doesn't exist.
create_bid_table(Nodes) ->
    case mnesia:create_table(bid, [
        {attributes, record_info(fields, bid)},
        {disc_copies, Nodes}
    ]) of
        {atomic, ok} ->
            logger:info("[mnesia_setup] create_bid_table => Bid table created!~n"),
            ok;
        {aborted, Reason} ->
            logger:info("[mnesia_setup] create_bid_table => Bid table not created: ~p~n", [Reason])
    end.
