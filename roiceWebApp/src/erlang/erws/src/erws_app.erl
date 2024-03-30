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
    % Retrieve WebSocket endpoint and port from application environment
    {ok, Url} = application:get_env(websocket_endpoint),
    {ok, Port} = application:get_env(websocket_port),

    % Compile Cowboy dispatch rules
    Dispatch = cowboy_router:compile([
        {'_', [
            {Url, erws_handler, []}
        ]}
    ]),

    % Start Cowboy server with clear options
    {ok, Pid} = cowboy:start_clear(erws,
        [{port, Port}],
        #{env => #{dispatch => Dispatch}}
    ),

    % Set logger level to debug
    logger:set_primary_config(level, debug),

    logger:info("[erws_app] start => cowboy is listening from process ~p~n", [Pid]),

    % Start the supervisor for handling WebSocket connections
    erws_sup:start_link().

% Stop function
stop(_State) ->
    ok.