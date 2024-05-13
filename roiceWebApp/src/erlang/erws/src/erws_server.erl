%%%-------------------------------------------------------------------
%%% @doc
%%%   This module is responsible for starting and configuring the WebSocket server
%%%   using Cowboy, a small, fast, and modern HTTP server for Erlang/OTP.
%%% @end
%%%-------------------------------------------------------------------
-module(erws_server).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Arg) ->
    {ok, Url} = application:get_env(websocket_endpoint),
    {ok, Port} = application:get_env(websocket_port),

    % Compile Cowboy dispatch rules
    Dispatch = cowboy_router:compile([
        {'_', [
            {Url, erws_auction_agent, []}
        ]}
    ]),

    % Start Cowboy server with clear options
    {ok, Pid} = cowboy:start_clear(erws,
        [{port, Port}],
        #{env => #{dispatch => Dispatch}}
    ),
    % Set logger level to debug
    logger:set_primary_config(level, info),
    logger:info("[erws_server] init => Cowboy is listening from process ~p~n", [Pid]),
    {ok, []}.

handle_call(Request, _From, State) ->
    {noreply, Request, State, hibernate}.

handle_cast(_Request, State) ->
    {noreply, State, hibernate}.

