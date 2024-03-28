%%%-------------------------------------------------------------------
%%% @doc
%%%   erws_handler module contains functions for handling HTTP requests
%%%   and WebSocket connections. It defines the behavior callbacks for
%%%   both HTTP and WebSocket.
%%% @end
%%%-------------------------------------------------------------------
-module(erws_handler).

-behaviour(cowboy_http_handler).
-behaviour(cowboy_websocket_handler).

-export([init/2, handle/2, terminate/3]).
-export([
  websocket_init/3, websocket_handle/2,
  websocket_info/3, websocket_terminate/3
]).

% Initialize WebSocket connection
init(Req, State) ->
  {cowboy_websocket, Req, State}.

% Handle HTTP requests
handle(Req, State) ->
  logger:debug("[erws_handler] handle => Request not expected: ~p", [Req]),
  {ok, Req2} = cowboy_http_req:reply(404, [{'Content-Type', <<"text/html">>}]),
  {ok, Req2, State}.

% Initialize WebSocket
websocket_init(_TransportName, Req, _Opts) ->
  logger:debug("[erws_handler] websocket_init => WebSocket initialized", []),
  {ok, Req, undefined_state}.

% Handle WebSocket text messages
websocket_handle({text, Msg}, State) ->
  logger:info("[erws_handler] websocket_handle => Frame: ~p, State: ~p~n", [Msg, State]),
  {reply, {text, <<Msg/binary>>}, State, hibernate};

% Handle other WebSocket messages (if necessary)
websocket_handle(_Any, State) ->
  {reply, {text, <<"whut?">>}, State, hibernate}.

% Handle WebSocket timeout messages
websocket_info({timeout, _Ref, Msg}, Req, State) ->
  {reply, {text, Msg}, Req, State};

% Handle other WebSocket info messages
websocket_info(_Info, Req, State) ->
  logger:debug("[erws_handler] websocket_info => websocket info"),
  {ok, Req, State, hibernate}.

% Terminate WebSocket connection
websocket_terminate(_Reason, _Req, _State) ->
  ok.

% Terminate function
terminate(_Reason, _Req, _State) ->
  ok.
