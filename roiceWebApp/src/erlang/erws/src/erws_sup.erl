%%%-------------------------------------------------------------------
%%% @doc
%%%   erws_sup module is the top-level supervisor for the erws application.
%%%   It starts the application supervisor with the appropriate child
%%%   specifications and supervision flags.
%%% @end
%%%-------------------------------------------------------------------
-module(erws_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

% Start the supervisor process
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

% Initialize the supervisor with supervision flags and child specs
init([]) ->
  SupFlags = #{strategy => one_for_one,
    intensity => 5,
    period => 10},
  ChildSpecs = [],
  {ok, {SupFlags, ChildSpecs}}.

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional