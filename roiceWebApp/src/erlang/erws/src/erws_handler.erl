%%%-------------------------------------------------------------------
%%% @doc
%%%
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

init(Req, State) ->
  {cowboy_websocket, Req, State}.

handle(Req, State) ->
  logger:debug("[erws_handler] websocket_init => Request not expected: ~p", [Req]),
  {ok, Req2} = cowboy_http_req:reply(404, [{'Content-Type', <<"text/html">>}]),
  {ok, Req2, State}.

websocket_init(_TransportName, Req, _Opts) ->
  logger:debug("[erws_handler] websocket_init => Websocket initialized", []),
  {ok, Req, undefined_state}.

websocket_handle({text, Msg}, State) ->
  logger:info("[erws_handler] websocket_handle => Frame: ~p, State: ~p~n", [Msg, State]),
  {reply, {text, <<Msg/binary>>}, State, hibernate};

websocket_handle(_Any, State) ->
  {reply, {text, <<"whut?">>}, State, hibernate}.

websocket_info({timeout, _Ref, Msg}, Req, State) ->
  {reply, {text, Msg}, Req, State};

websocket_info(_Info, Req, State) ->
  logger:debug("[erws_handler] websocket_handle => websocket info"),
  {ok, Req, State, hibernate}.

websocket_terminate(_Reason, _Req, _State) ->
  ok.

terminate(_Reason, _Req, _State) ->
  ok.

