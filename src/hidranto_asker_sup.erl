-module(hidranto_asker_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% API functions
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% Supervisor callbacks
init([]) ->
    Type = simple_one_for_one,
    MaxRestarts = 5,
    MaxSeconds = 10,
    TypeSup = {hidranto_asker_worker,
               {hidranto_asker_worker, start_link, []},
               transient,
               5000, % try to shutdown gracefully, kill after 5secs.
               supervisor,
               [hidranto_asker_worker]},
    {ok, {{Type, MaxRestarts, MaxSeconds},
          [TypeSup]}}.
