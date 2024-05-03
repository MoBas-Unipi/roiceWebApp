%%%-------------------------------------------------------------------
%% @doc master public API
%% @end
%%%-------------------------------------------------------------------

-module(master_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    % Connect to cluster nodes
    {ok, Nodes} = application:get_env(nodes),
    logger:info("[master] start => Nodes ~p~n", [Nodes]),
    connect_nodes(Nodes),

    % Init Mnesia
    logger:info("[master] start => Init Mnesia~n"),
    mnesia_setup:init(Nodes),

    % Start application on remote nodes
    start_application(Nodes),

    {ok, self(), Nodes}.


%% Connect to remote nodes
connect_nodes([]) ->
    ok;

connect_nodes([H | T]) ->
    logger:info("[master] connect_nodes => Connected node ~p~n", [H]),
    true = net_kernel:connect_node(H),
    connect_nodes(T).


%% Start remote nodes
start_application([]) ->
    ok;

start_application([Node | T]) ->
    logger:info("[master] Started node => ~p~n", [Node]),
    spawn(Node, application, start, [erws]),
    start_application(T).


stop(Nodes) ->
    % Stop Mnesia
    spawn(mnesia, stop, []),
    % Stop remote nodes
    stop_nodes(Nodes),
    ok.

%% Stop remote nodes
stop_nodes([]) ->
    ok;

stop_nodes([Node | T]) ->
    logger:info("[master] stop_nodes => ~p~n", [Node]),
    spawn(Node, application, stop, [erws]),
    stop_nodes(T).

