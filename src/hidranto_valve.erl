%% TODO: remove this

-module(hidranto_valve).
-export([init/0, open/1, attempt/1]).

-define(MAX_VALVE, 1000).

init() ->
    ets:new(hidranto_valves, [public, named_table,
                              {write_concurrency, true}]).

open(Valve) ->
    ets:insert(hidranto_valves, {Valve, 0, os:timestamp()}).

attempt(Valve) ->
    case catch ets:update_counter(hidranto_valves, Valve,
                            {2, 1, ?MAX_VALVE, 1}) of
        1                     -> true;
        X when is_integer(X)  -> false;
        {'EXIT', {badarg, _}} -> open(Valve), attempt(Valve)
    end.
