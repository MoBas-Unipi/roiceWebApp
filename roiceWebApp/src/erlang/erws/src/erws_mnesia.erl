-module(erws_mnesia).
-export([setup_tables/0, create_auction_table/0,save_auction_pid/1,print_mnesia_content/0]).

-record(auction, {pid, start_date, end_date}).

setup_tables() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    create_auction_table().

create_auction_table() ->
    case mnesia:create_table(auction, [{attributes, record_info(fields, auction)}]) of
        {atomic, ok} ->
            logger:info("Table created! ~n"),
            ok;
        {atomic, {already_exists, _Table}} ->
            logger:info("Table already exists: ~p~n", [_Table]),
            ok;
        {aborted, Reason} ->
            logger:info("Error creating auction table: ~p~n", [Reason])
    end.


save_auction_pid(AuctionPid) ->
    F = fun() ->
        mnesia:write(#auction{pid=AuctionPid, start_date = [], end_date = []})
        end,
    mnesia:activity(transaction, F).


% Funzione per stampare il contenuto del database Mnesia
print_mnesia_content() ->
    F = fun() ->
        case mnesia:dirty_all_keys(auction) of
            [] ->
                logger:info("No auction Pids found in the table~n");
            Pids ->
                logger:info("Auction Pids found in the table: ~p~n", [Pids])
        end
        end,
    mnesia:activity(transaction, F).



