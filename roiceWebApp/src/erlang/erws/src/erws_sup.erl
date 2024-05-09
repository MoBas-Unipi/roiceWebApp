%%%-------------------------------------------------------------------
%%% @doc
%%%   erws_sup module is the top-level supervisor for the erws application.
%%%   It starts the application supervisor with the appropriate child
%%%   specifications and supervision flags.
%%% @end
%%%-------------------------------------------------------------------
-module(erws_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% @private
%% @doc Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
init([]) ->
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 3600,
    RestartStrategy = one_for_one,

    SupFlags = #{strategy => RestartStrategy,
                intensity => MaxRestarts,
                period => MaxSecondsBetweenRestarts},

    ChildSpecs = [
        #{
            id => erws_server,
            start => {erws_server, start_link, []},
            restart => permanent,
            shutdown => 2000,
            type => worker,
            modules => [erws_server]
        },
        #{
            id => erws_dynamic_sup,
            start => {erws_dynamic_sup, start_link, []},
            restart => permanent,
            shutdown => 2000,
            type => supervisor,
            modules => [erws_dynamic_sup]
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.

%%%%% Function to start an auction process
%%start_auction_process(PhoneName, MinimumPrice, AuctionTime, EndDate) ->
%%    {A, B, _C} = now(),
%%    MY_ID = A * 1000000 + B,
%%    logger:info("[erws_sup] start_auction_process => called for: ~p, time: ~p ~n", [PhoneName, MY_ID]),
%%     case supervisor:start_child(erws_sup,
%%         #{
%%             id => PhoneName,
%%             start => {erws_auction_agent, auction_handle, [PhoneName, MinimumPrice, AuctionTime, EndDate]},
%%             restart => transient
%%         }
%%     ) of
%%         {ok, AuctionPid} ->
%%             logger:info("[erws_sup] start_auction_process => Start child executed correctly, PID: ~p~n", [AuctionPid]),
%%             AuctionPid;
%%         Error ->
%%             logger:info("[erws_sup] start_auction_process => Error: ~p~n", [Error]),
%%             Error
%%     end.
%%
%%% Function to start a bidder process
%%start_bidder_process(AuctionPid, PhoneName) ->
%%%%    RandomId = random_id(), % Generate a random ID
%%%%    Id = <<PhoneName/binary, "-", integer_to_binary(RandomId)>>, % Concatenate PhoneName and RandomId
%%    case supervisor:start_child(?SERVER,
%%        #{
%%            id => bidder,
%%            start => {erws_bidder_handler, init_bidder, [AuctionPid, PhoneName]},
%%            restart => transient
%%        }
%%    ) of
%%        {ok, BidderPid} ->
%%            BidderPid;
%%        Error ->
%%            Error
%%    end.
%%
%%% Function to generate a random ID
%%random_id() ->
%%    random:uniform(math:pow(2, 31) - 1). % Assuming a random ID within the range of 0 to 2^31 - 1
