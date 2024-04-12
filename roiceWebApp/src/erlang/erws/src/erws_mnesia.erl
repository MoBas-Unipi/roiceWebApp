-module(erws_mnesia).
-export([setup_tables/0, create_auction_table/0,save_auction/2,print_auctions/0, get_auction_pid/1,save_bid/4, print_bids/0]).

-record(auction,{phone_name, auction_pid}).
-record(bid, {phone_name, current_winner_user_email, bid_date, bid_value}).

setup_tables() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    create_auction_table(),
    create_bid_table().

%-------------------------AUCTION Table Functions------------------------%

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

save_auction(PhoneName, AuctionPid) ->
    F = fun() ->
        mnesia:write(#auction{phone_name = PhoneName, auction_pid = AuctionPid})
        end,
    mnesia:activity(transaction, F).

print_auctions() ->
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
            logger:error("Auction for the Phone ~p not found in the table~n", [Key]),
            process_auction_list(Rest);
        [Auction] ->
            print_auction(Auction),
            process_auction_list(Rest)
    end.

print_auction(#auction{phone_name = PhoneName, auction_pid = AuctionPid}) ->
    logger:info("Auction found in the table: Phone Name: ~p, Auction Pid: ~p~n", [PhoneName, AuctionPid]).

get_auction_pid(PhoneName) ->
    {atomic, Result} = mnesia:transaction(fun() ->
        case mnesia:read(auction, PhoneName) of
            [AuctionRecord] ->
                AuctionRecord#auction.auction_pid;
            [] ->
                not_found
        end
                                          end),
    Result.


%-------------------------BID Table Functions------------------------%

create_bid_table() ->
    case mnesia:create_table(bid, [{attributes, record_info(fields, bid)}]) of
        {atomic, ok} ->
            logger:info("Bid table created!~n"),
            ok;
        {atomic, {already_exists, _Table}} ->
            logger:info("Bid table already exists: ~p~n", [_Table]),
            ok;
        {aborted, Reason} ->
            logger:info("Error creating bid table: ~p~n", [Reason])
    end.



save_bid(PhoneName, WinnerEmail, BidDate, BidValue) ->
    F = fun() ->
        mnesia:write(#bid{phone_name=PhoneName, current_winner_user_email=WinnerEmail, bid_date=BidDate, bid_value=BidValue})
    end,
    mnesia:activity(transaction, F).



print_bids() ->
    F = fun() ->
        case mnesia:dirty_all_keys(bid) of
            [] ->
                logger:info("No bids found in the table~n");
            Bids ->
                process_list(Bids)
        end
    end,
    mnesia:activity(transaction, F).

process_list([]) ->
    done;
process_list([Key | Rest]) ->
    case mnesia:dirty_read({bid, Key}) of
        [] ->
            logger:error("Bid with key ~p not found in the table~n", [Key]),
            process_list(Rest);
        [Bid] ->
            print_bid(Bid),
            process_list(Rest)
    end.

print_bid(#bid{phone_name = PhoneName, current_winner_user_email = WinnerEmail, bid_date = BidDate, bid_value = BidValue}) ->
    logger:info("Bid found in the table: Phone Name: ~p, Winner Email: ~p, Bid Date: ~p, Bid Value: ~p~n", [PhoneName, WinnerEmail, BidDate, BidValue]).


