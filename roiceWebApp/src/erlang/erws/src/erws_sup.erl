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
            shutdown => 5000,
            type => worker,
            modules => [erws_server]
        },
        #{
            id => erws_dynamic_sup,
            start => {erws_dynamic_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor,
            modules => [erws_dynamic_sup]
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.