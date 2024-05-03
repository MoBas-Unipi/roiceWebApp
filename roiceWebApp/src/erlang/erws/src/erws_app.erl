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
    mnesia:start(),
%%    ok = mnesia:wait_for_tables([auction], 30000),
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

%%start(_StartType, _StartArgs) ->
%%    % Setup Mnesia tables
%%    case erws_mnesia:setup_tables() of
%%        ok ->
%%            % Continue with starting the application
%%            {ok, Url} = application:get_env(websocket_endpoint),
%%            {ok, Port} = application:get_env(websocket_port),
%%
%%            % Compile Cowboy dispatch rules
%%            Dispatch = cowboy_router:compile([
%%                {'_', [
%%                    {Url, erws_auction_agent, []}
%%                ]}
%%            ]),
%%
%%            % Start Cowboy server with clear options
%%            {ok, Pid} = cowboy:start_clear(erws,
%%                [{port, Port}],
%%                #{env => #{dispatch => Dispatch}}
%%            ),
%%
%%            % Set logger level to debug
%%            logger:set_primary_config(level, debug),
%%
%%            logger:info("[erws_app] start => cowboy is listening from process ~p~n", [Pid]),
%%
%%            % Start the supervisor for handling WebSocket connections
%%            erws_sup:start_link();
%%
%%        {error, Reason} ->
%%            % Handle the error appropriately
%%            logger:error("[erws_app] start => Error setting up Mnesia tables: ~p~n", [Reason]),
%%            {error, Reason}
%%    end.
