%%%-------------------------------------------------------------------
%%% @doc
%%%   erws_app module is the main application module responsible for
%%%   starting and stopping the application. It initializes the WebSocket
%%%   endpoint and starts the main Cowboy server.
%%% @end
%%%-------------------------------------------------------------------

-module(erws_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    % Start the supervisor for handling WebSocket connections
    case erws_sup:start_link() of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            Error
    end.


% Stop function
stop(_State) ->
    ok.
