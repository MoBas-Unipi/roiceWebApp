%%%-------------------------------------------------------------------
%%% @doc
%%%   erws_dynamic_sup module manages auction processes supervision.
%%%   It starts auction processes, sets restart strategies, and specifies child
%%%   specifications for auction processes.
%%% @end
%%%-------------------------------------------------------------------
-module(erws_dynamic_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1, start_auction_process/4, terminate_auction_process/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @private
%% @doc Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child specifications.
init([]) ->
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 3600,
    SupFlags = #{strategy => simple_one_for_one,
                intensity => MaxRestarts,
                period => MaxSecondsBetweenRestarts},
    ChildSpecs = [
        #{
            id => erws_auction_handler,
            start => {erws_auction_handler, start_link, []},
            restart => transient,
            shutdown => 5000,
            type => worker,
            modules => [erws_auction_handler]
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.


%%% Function to start an auction process
start_auction_process(PhoneName, MinimumPrice, AuctionTime, EndDate) ->
    logger:debug("[erws_dynamic_sup] start_auction_process => called for: ~p, time: ~p ~n", [PhoneName, erlang:system_time()]),

    case supervisor:start_child(?MODULE,
        [PhoneName, MinimumPrice, AuctionTime, EndDate]
    ) of
        {ok, AuctionPid} ->
            logger:info("[erws_dynamic_sup] start_auction_process => Start child executed correctly, PID: ~p~n", [AuctionPid]),
            AuctionPid;
        Error ->
            logger:error("[erws_dynamic_sup] start_auction_process => Error: ~p~n", [Error]),
            Error
    end.

terminate_auction_process(ChildId) ->
    supervisor:terminate_child(?MODULE, ChildId).

