%%%-------------------------------------------------------------------
%%% @doc
%%%   erws_sup module is the top-level supervisor for the erws application.
%%%   It starts the application supervisor with the appropriate child
%%%   specifications and supervision flags.
%%% @end
%%%-------------------------------------------------------------------
-module(erws_sup).

-behaviour(supervisor).

-export([start_link/0, init/1, start_auction_process/4, start_bidder_process/2, random_id/0]).

-define(SERVER, ?MODULE).

% Start the supervisor process
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

% Initialize the supervisor with supervision flags and child specs

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_one,
        intensity => 5,
        period => 30},
    ChildSpecs = [
        #{
            id => erws_server,
            start => {erws_server, start_link, []},
            restart => permanent
        }],
    {ok, {SupFlags, ChildSpecs}}.

%%% Function to start an auction process
start_auction_process(PhoneName, MinimumPrice, AuctionTime, EndDate) ->
    {ok, AuctionPid} = supervisor:start_child(?SERVER,
        #{
            id => PhoneName,
            start => {erws_auction_agent, auction_handle, [PhoneName, MinimumPrice, AuctionTime, EndDate]},
            restart => transient
        }
    ),
    AuctionPid.

% Function to start a bidder process
start_bidder_process(AuctionPid, PhoneName) ->
%%    RandomId = random_id(), % Generate a random ID
%%    Id = <<PhoneName/binary, "-", integer_to_binary(RandomId)>>, % Concatenate PhoneName and RandomId
    {ok, BidderPid} = supervisor:start_child(?SERVER,
        #{
            id => bidder,
            start => {erws_bidder_handler, start, [AuctionPid, PhoneName]},
            restart => transient
        }),
    BidderPid.

% Function to generate a random ID
random_id() ->
    random:uniform(math:pow(2, 31) - 1). % Assuming a random ID within the range of 0 to 2^31 - 1
