-module(erws_mnesia).
-export([
    setup_tables/0,
    create_auction_table/0,
    save_auction/2,
    print_auctions/0,
    get_winner_bidder/1,
    add_bidder_to_auction/2,
    get_auction_pid/1,
    get_auction_bidders/1,
    is_bidder_present/2,
    delete_bidder/2,
    save_bid/3,
    print_bids/0,
    create_bidder_table/0,
    save_bidder/2,
    get_bidder_pid/1
]).

-record(auction,{phone_name, auction_pid, auction_bidders}).
-record(bid, {phone_name, current_winner_user_email, bid_value}).
-record(bidder, {bidder_email, bidder_pid}).

setup_tables() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    create_auction_table(),
    create_bid_table(),
    create_bidder_table().

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
        mnesia:write(#auction{phone_name = PhoneName, auction_pid = AuctionPid, auction_bidders = []})
        end,
    mnesia:activity(transaction, F).

print_auctions() ->
    F = fun() ->
        case mnesia:dirty_all_keys(auction) of
            [] ->
                logger:info("No auctions found in the table~n");
            Auctions ->
                process_auction_list(Auctions)
        end
        end,
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

print_auction(#auction{phone_name = PhoneName, auction_pid = AuctionPid, auction_bidders =  BidderPids}) ->
    logger:info("Auction found in the table: Phone Name: ~p, Auction Pid: ~p, Bidder PIDs: ~p~n", [PhoneName, AuctionPid, BidderPids]).

add_bidder_to_auction(PhoneName, Bidder) ->
    F = fun() ->
        case mnesia:read(auction, PhoneName) of
            [AuctionRecord] ->
                UpdatedBidders = [Bidder | AuctionRecord#auction.auction_bidders],
                UpdatedAuctionRecord = AuctionRecord#auction{auction_bidders = UpdatedBidders},
                mnesia:write(UpdatedAuctionRecord),
                UpdatedBidders; % Return the updated list of bidders
            [] ->
                % Create a new auction record with the bidder added
                UpdatedBidders = [Bidder | []],
                UpdatedAuctionRecord = #auction{auction_bidders = UpdatedBidders},
                mnesia:write(UpdatedAuctionRecord),
                UpdatedBidders % Return the new list of bidders
        end
        end,
    mnesia:activity(transaction, F).




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


get_auction_bidders(PhoneName) ->
    {atomic,Result} = mnesia:transaction(fun() ->
        % Retrieve the auction record based on the provided AuctionPid
        case mnesia:read(auction, PhoneName) of
            [AuctionRecord] ->
                % Extract the list of bidder PIDs from the auction record
                AuctionRecord#auction.auction_bidders;
            [] ->
                % If the auction record is not found, return an empty list
                []
        end
                       end),
    Result.

% Function to check if a bidder is present in the list of bidders for an auction
% Function to check if a bidder is present in the list of bidders for an auction
is_bidder_present(PhoneName, Email) ->
    F = fun() ->
        case mnesia:read(auction, PhoneName) of
            [BidderRecord] ->
                Bidders = BidderRecord#auction.auction_bidders,
                logger:info("List of BIDDERS: ~p~n", [Bidders]),
                is_bidder_present_in_auction(Bidders, Email);
            [] ->
                logger:info("List of BIDDERS empty ~n"),
                false % Bidder not found
        end
        end,
    mnesia:activity(transaction, F).

is_bidder_present_in_auction(Bidders, Email) ->
    lists:any(fun({_, BidderEmail, _}) -> string:equal(BidderEmail,Email) end, Bidders).



delete_bidder(BidderPid, PhoneName) ->
    F = fun() ->
        case mnesia:read({auction, PhoneName}) of
            [AuctionRecord] ->
                UpdatedBidders = lists:keydelete(BidderPid, 1, AuctionRecord#auction.auction_bidders),
                UpdatedAuctionRecord = AuctionRecord#auction{auction_bidders = UpdatedBidders},
                mnesia:write(UpdatedAuctionRecord),
                UpdatedBidders; % Return the updated list of bidders
            [] ->
                not_found % Auction record not found
        end
        end,
    mnesia:activity(transaction, F).

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


% Saves a bid entry into the Mnesia table.
save_bid(PhoneName, WinnerEmail, BidValue) ->
    F = fun() ->
        % Write the bid entry into the Mnesia bid table
        mnesia:write(#bid{phone_name=PhoneName, current_winner_user_email=WinnerEmail, bid_value=BidValue})
    end,
    % Perform the write operation within a transaction
    mnesia:activity(transaction, F).

get_winner_bidder(PhoneName) ->
    {atomic, Result} = mnesia:transaction(fun() ->
        case mnesia:read(bid, PhoneName) of
            [BidRecord] ->
                {BidRecord#bid.current_winner_user_email, BidRecord#bid.bid_value};
            [] ->
                not_found
        end
                                          end),
    Result.



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
print_bid(#bid{phone_name = PhoneName, current_winner_user_email = WinnerEmail, bid_value = BidValue}) ->
    % Display a log message with information about the bid
    logger:info("Bid found in the table: Phone Name: ~p, Winner Email: ~p, Bid Value: ~p~n", [PhoneName, WinnerEmail, BidValue]).


% Get bid record information by phone name from BID table of MNESIA DB
%%get_bid(PhoneName) ->
%%    %% Avvio di una transazione per leggere dalla tabella bid
%%    {atomic, Result} = mnesia:transaction(fun() ->
%%        %% Utilizzo di mnesia:read per leggere il record dalla tabella bid
%%        case mnesia:read(bid, PhoneName) of
%%            [BidRecord] ->
%%                BidRecord;
%%            [] ->
%%                not_found
%%        end
%%    end),
%%    Result.


%-------------------------BIDDER Table Functions------------------------%

create_bidder_table() ->
    case mnesia:create_table(bidder, [{attributes, record_info(fields, bidder)}]) of
        {atomic, ok} ->
            logger:info("Bidder table created!~n"),
            ok;
        {atomic, {already_exists, _Table}} ->
            logger:info("Bidder table already exists: ~p~n", [_Table]),
            ok;
        {aborted, Reason} ->
            logger:error("Error creating bidder table: ~p~n", [Reason])
    end.


save_bidder(BidderEmail, BidderPid) ->
    F = fun() ->
        mnesia:write(#bidder{bidder_email = BidderEmail, bidder_pid = BidderPid})
    end,
    mnesia:activity(transaction, F).


get_bidder_pid(BidderEmail) ->
    {atomic, Result} = mnesia:transaction(fun() ->
        case mnesia:read(bidder, BidderEmail) of
            [BidderRecord] ->
                BidderRecord#bidder.bidder_pid;
            [] ->
                not_found
        end
    end),
    Result.