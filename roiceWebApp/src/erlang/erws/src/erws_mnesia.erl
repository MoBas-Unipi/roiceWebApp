-module(erws_mnesia).
-export([setup_tables/0, create_auction_table/0, save_auction/2, print_mnesia_content/0]).

-record(auction,{auction_pid, phone_name}).

setup_tables() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    create_auction_table().

create_auction_table() ->
    case mnesia:create_table(auction, [{attributes, record_info(fields, auction)}]) of
        {atomic, ok} ->
            logger:info("Auction table created!~n"),
            ok;
        {atomic, {already_exists, _Table}} ->
            logger:info("Auction table already exists: ~p~n", [_Table]),
            ok;
        {aborted, Reason} ->
            logger:error("Error creating auction table: ~p~n", [Reason])
    end.

save_auction(AuctionPid, PhoneName) ->
    F = fun() ->
        mnesia:write(#auction{auction_pid = AuctionPid, phone_name = PhoneName})
        end,
    mnesia:activity(transaction, F).

print_mnesia_content() ->
    F = fun() ->
        case mnesia:dirty_all_keys(auction) of
            [] ->
                logger:info("No auctions found in the table~n");
            Auctions ->
                process_auction_list(Auctions)
        end end,
    mnesia:activity(transaction, F).

process_auction_list([]) ->
    done;
process_auction_list([Key | Rest]) ->
    case mnesia:dirty_read({auction, Key}) of
        [] ->
            logger:error("Auction with key ~p not found in the table~n", [Key]),
            process_auction_list(Rest);
        [Auction] ->
            print_auction(Auction),
            process_auction_list(Rest)
    end.

print_auction(#auction{ auction_pid = AuctionPid, phone_name = PhoneName}) ->
    logger:info("Auction found in the table: Auction Pid: ~p, Phone Name: ~p~n", [AuctionPid, PhoneName]).
