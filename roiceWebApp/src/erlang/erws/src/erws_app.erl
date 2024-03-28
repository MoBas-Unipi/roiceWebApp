%%%-------------------------------------------------------------------
%% @doc erws public API
%% @end
%%%-------------------------------------------------------------------

-module(erws_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, Url} = application:get_env(websocket_endpoint),
    {ok, Port} = application:get_env(websocket_port),

    Dispatch = cowboy_router:compile([
        {'_', [
            {Url, erws_handler, []}
        ]}
    ]),

    {ok, Pid} = cowboy:start_clear(erws,
        [{port, Port}],
        #{env => #{dispatch => Dispatch}}
    ),

    % Set logger level
    logger:set_primary_config(level, debug),

    logger:info("[erws_app] start => cowboy is listening from process ~p~n", [Pid]),

    erws_sup:start_link().

stop(_State) ->
    ok.