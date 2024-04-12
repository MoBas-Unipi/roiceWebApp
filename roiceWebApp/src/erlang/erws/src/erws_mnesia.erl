-module(erws_mnesia).
-export([setup_tables/0,create_auction_table/0,save_auction/2,print_mnesia_content/0,save_bid/4, print_bids/0, get_bid/1]).

-record(auction,{auction_pid, phone_name}).
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

%-------------------------BID Table Functions------------------------%

% Creates or checks existence of Mnesia bid table.
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


% Saves a bid entry into the Mnesia table.
save_bid(PhoneName, WinnerEmail, BidDate, BidValue) ->
    F = fun() ->
        % Write the bid entry into the Mnesia bid table
        mnesia:write(#bid{phone_name=PhoneName, current_winner_user_email=WinnerEmail, bid_date=BidDate, bid_value=BidValue})
    end,
    % Perform the write operation within a transaction
    mnesia:activity(transaction, F).


% Prints all bids stored in the Mnesia table.
print_bids() ->
    F = fun() ->
        % Retrieves all keys from the Mnesia bid table
        case mnesia:dirty_all_keys(bid) of
            [] ->
                % Display a log message if no bids are found
                logger:info("No bids found in the table~n");
            Bids ->
                % Call the process_list function to print each bid if bids are found
                process_list(Bids)
        end
    end,
    % Execute the defined function within a transaction
    mnesia:activity(transaction, F).

% Processes a list of bid keys retrieved from the Mnesia table.
process_list([]) ->
    % If the list is empty, processing is complete
    done;
process_list([Key | Rest]) ->
    % If the list is not empty, process the first key in the list
    case mnesia:dirty_read({bid, Key}) of
        [] ->
            % Display an error message if the bid with the current key is not found
            logger:error("Bid with key ~p not found in the table~n", [Key]),
            % Continue processing the rest of the keys
            process_list(Rest);
        [Bid] ->
            % Print bid information if the bid is found
            print_bid(Bid),
            % Continue processing the rest of the keys
            process_list(Rest)
    end.

% Prints information about a single bid entry.
print_bid(#bid{phone_name = PhoneName, current_winner_user_email = WinnerEmail, bid_date = BidDate, bid_value = BidValue}) ->
    % Display a log message with information about the bid
    logger:info("Bid found in the table: Phone Name: ~p, Winner Email: ~p, Bid Date: ~p, Bid Value: ~p~n", [PhoneName, WinnerEmail, BidDate, BidValue]).


% Get bid record information by phone name from BID table of MNESIA DB
get_bid(PhoneName) ->
    %% Avvio di una transazione per leggere dalla tabella bid
    {atomic, Result} = mnesia:transaction(fun() ->
        %% Utilizzo di mnesia:read per leggere il record dalla tabella bid
        case mnesia:read(bid, PhoneName) of
            [BidRecord] ->
                BidRecord;
            [] ->
                not_found
        end
    end),
    Result.