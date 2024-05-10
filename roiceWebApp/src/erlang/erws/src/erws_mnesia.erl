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
    mnesia:activity(transaction, F).

% Function to get the auction PID by phone name.
get_auction_pid(PhoneName) ->
    F = fun() ->
            case mnesia:read(auction, PhoneName) of
                [AuctionRecord] ->
                    AuctionRecord#auction.auction_pid;
                [] ->
                    not_found
            end
        end,
    mnesia:activity(transaction, F).

% Function to delete an auction based on phone_name.
delete_auction(PhoneName) ->
    F = fun() ->
            mnesia:delete({auction, PhoneName})
        end,
    mnesia:activity(transaction, F).


%-------------------------BID Table Functions------------------------%

% Function to save a bid entry into the Mnesia bid table.
save_bid(PhoneName, WinnerEmail, BidValue) ->
    F = fun() ->
            mnesia:write(#bid{phone_name = PhoneName, current_winner_user_email = WinnerEmail, bid_value = BidValue}),
            logger:info("[erws_mnesia] save_bid => Bid saved, user: ~p, phone: ~p, value: ~p ~n", [WinnerEmail, PhoneName, BidValue])
        end,
    mnesia:activity(transaction, F).

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
    mnesia:activity(transaction, F).

% Function to delete a bid based on phone_name.
delete_bid(PhoneName) ->
    F = fun() ->
            mnesia:delete({bid, PhoneName}),
            logger:info("[erws_mnesia] delete_bid => Bid deleted for ~p: ~n", [PhoneName])
        end,
    mnesia:activity(transaction, F).


