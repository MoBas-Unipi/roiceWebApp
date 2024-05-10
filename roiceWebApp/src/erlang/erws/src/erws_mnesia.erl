-module(erws_mnesia).
-export([
    save_auction/2,
    get_auction_pid/1,
    delete_auction/1,

    save_bid/3,
    get_winner_bidder/1,
    delete_bid/1
]).

-record(auction, {phone_name, auction_pid}).
-record(bid, {phone_name, current_winner_user_email, bid_value}).

%-------------------------AUCTION Table Functions------------------------%

% Function to save an auction record.
save_auction(PhoneName, AuctionPid) ->
    F = fun() ->
            mnesia:write(#auction{phone_name = PhoneName, auction_pid = AuctionPid})
        end,
    case mnesia:activity(transaction, F) of
        {atomic, Result} ->
            logger:info("[erws_mnesia] save_auction => Result: ~p for ~p with PID ~p ~n", [Result, PhoneName, AuctionPid]),
            ok;
        {aborted, Reason} ->
            logger:error("[erws_mnesia] save_auction => save_auction failed: ~p~n", [Reason]),
            exit({aborted, Reason});
        ok ->
            ok
    end.

% Function to get the auction PID by phone name.
get_auction_pid(PhoneName) ->
    F = fun() ->
            case mnesia:read(auction, PhoneName) of
                [AuctionRecord] ->
                    AuctionRecord#auction.auction_pid;
                [] ->
                    logger:error("[erws_mnesia] get_auction_pid => get_auction_pid failed: PID not found"),
                    exit({aborted, "PID not found"})
            end
        end,
    case mnesia:activity(transaction, F) of
        {atomic, AuctionPid} ->
            logger:info("[erws_mnesia] get_auction_pid => Result of get_auction_pid for the phone ~p: ~p~n",
                [PhoneName, AuctionPid]),
            {ok, AuctionPid};
        {aborted, Reason} ->
            logger:error("[erws_mnesia] get_auction_pid => get_auction_pid failed: ~p~n", [Reason]),
            exit({aborted, Reason});
        AuctionPid ->
            logger:info("[erws_mnesia] get_auction_pid => Result of get_auction_pid for the phone ~p: ~p~n", [PhoneName, AuctionPid]),
            {ok, AuctionPid}
    end.

% Function to delete an auction based on phone_name.
delete_auction(PhoneName) ->
    F = fun() ->
            mnesia:delete({auction, PhoneName})
        end,
    case mnesia:activity(transaction, F) of
        {atomic, Result} ->
            logger:info("[erws_mnesia] delete_auction for the phone ~p => Result: ~p~n", [PhoneName, Result]),
            ok;
        {aborted, Reason} ->
            logger:error("[erws_mnesia] delete_auction => delete_auction failed: ~p~n", [Reason]),
            exit({aborted, Reason})
    end.


%-------------------------BID Table Functions------------------------%

% Function to save a bid entry into the Mnesia bid table.
save_bid(PhoneName, WinnerEmail, BidValue) ->
    F = fun() ->
            mnesia:write(#bid{phone_name = PhoneName, current_winner_user_email = WinnerEmail, bid_value = BidValue})
        end,
    case mnesia:activity(transaction, F) of
        {atomic, Result} ->
            logger:info("[erws_mnesia] save_bid => Bid of ~p from ~p for ~p, result: ~p ~n", [BidValue, WinnerEmail, PhoneName, Result]),
            ok;
        {aborted, Reason} ->
            logger:error("[erws_mnesia] save_bid => save_auction failed: ~p~n", [Reason]),
            exit({aborted, Reason})
    end.

% Function to get the winner bidder's email and bid value by phone name.
get_winner_bidder(PhoneName) ->
    F = fun() ->
            case mnesia:read(bid, PhoneName) of
                [BidRecord] ->
                    {BidRecord#bid.current_winner_user_email, BidRecord#bid.bid_value};
                [] ->
                    not_found
            end
        end,
    case mnesia:activity(transaction, F) of
        {atomic, CurrentWinner} ->
            logger:info("[erws_mnesia] get_winner_bidder =>
                            Result of get_winner_bidder for the phone ~p: ~p~n", [PhoneName, CurrentWinner]),
            CurrentWinner;
        {aborted, Reason} ->
            logger:error("[erws_mnesia] get_winner_bidder => get_auction_pid failed: ~p~n", [Reason]),
            exit({aborted, Reason})
    end.

% Function to delete a bid based on phone_name.
delete_bid(PhoneName) ->
    F = fun() ->
            mnesia:delete({bid, PhoneName})
        end,
    case mnesia:activity(transaction, F) of
        {atomic, Result} ->
            logger:info("[erws_mnesia] delete_bid for the phone ~p => Result: ~p~n", [PhoneName, Result]),
            ok;
        {aborted, Reason} ->
            logger:error("[erws_mnesia] delete_bid => delete_bid failed: ~p~n", [Reason]),
            exit({aborted, Reason})
    end.

