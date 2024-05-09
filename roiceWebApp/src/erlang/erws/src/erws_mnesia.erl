-module(erws_mnesia).
-export([
    save_auction/2,
    save_bid/3,
    get_winner_bidder/1,
    get_auction_pid/1
]).

-record(auction, {phone_name, auction_pid}).
-record(bid, {phone_name, current_winner_user_email, bid_value}).

%-------------------------AUCTION Table Functions------------------------%

% Function to save an auction record.
%%save_auction(PhoneName, AuctionPid) ->
%%    F = fun() ->
%%            case mnesia:read(auction, PhoneName) of
%%                [#auction{auction_pid = ExistingPid}] ->
%%                    % PhoneName already exists, update the auction_pid if different
%%                    if
%%                        ExistingPid =:= AuctionPid ->
%%                            logger:info("[erws_mnesia] => Phone already saved with the current PID"),
%%                            ok; % No need to update
%%                        true ->
%%                            logger:info("[erws_mnesia] => Phone already saved with PID: ~p, now saved as: ~p ~n", [ExistingPid, AuctionPid]),
%%                            mnesia:write(#auction{phone_name = PhoneName, auction_pid = AuctionPid})
%%                    end;
%%                [] ->
%%                    logger:info("[erws_mnesia] => Phone not present, saved with PID: ~p~n", [AuctionPid]),
%%                    % PhoneName doesn't exist, create a new record
%%                    mnesia:write(#auction{phone_name = PhoneName, auction_pid = AuctionPid})
%%            end
%%        end,
%%    mnesia:activity(transaction, F).

% Function to save an auction record.
save_auction(PhoneName, AuctionPid) ->
    F = fun() ->
            mnesia:write(#auction{phone_name = PhoneName, auction_pid = AuctionPid}),
            logger:info("[erws_mnesia] save_auction => Phone ~p saved with PID: ~p ~n", [PhoneName, AuctionPid])
        end,
    Result = mnesia:activity(sync_transaction, F),
    logger:info("[erws_mnesia] save_auction => Result of save: ~p~n", [Result]).


% Function to get the auction PID by phone name.
get_auction_pid(PhoneName) ->
    {atomic, Result} = mnesia:transaction(
        fun() ->
            case mnesia:read(auction, PhoneName) of
                [AuctionRecord] ->
                    AuctionRecord#auction.auction_pid;
                [] ->
                    not_found
            end
        end),
    Result.

%-------------------------BID Table Functions------------------------%

% Function to save a bid entry into the Mnesia bid table.
save_bid(PhoneName, WinnerEmail, BidValue) ->
    F = fun() ->
            mnesia:write(#bid{phone_name = PhoneName, current_winner_user_email = WinnerEmail, bid_value = BidValue})
        end,
    mnesia:activity(sync_transaction, F).

% Function to get the winner bidder's email and bid value by phone name.
get_winner_bidder(PhoneName) ->
    {atomic, Result} = mnesia:transaction(
        fun() ->
            case mnesia:read(bid, PhoneName) of
                [BidRecord] ->
                    {BidRecord#bid.current_winner_user_email, BidRecord#bid.bid_value};
                [] ->
                    not_found
            end
        end),
    Result.
