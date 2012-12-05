-module(hidranto).
-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-compile([export_all]).

-define(SERVER, ?MODULE).

-define(SIMULTANEOUS_REQS, 3).

-record(state, {}).

%%% API

start() ->
    [application:start(App) || App <- [compiler, syntax_tools, lager,
                                       gproc,
                                       hidranto]].

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

test(Asker) ->
    case ask(Asker, t) of
        {ok, Ref} -> spawn(fun() -> timer:sleep(5000), hidranto:done(Ref) end);
        Other -> Other
    end.

ask(Asker, Triplets) ->
    precheck_asker_count(Asker, Triplets).

precheck_asker_count(Asker, Triplets) ->
    %% NOTE(Dmitry): maybe it's better to skip lookup to impore ETS
    %% performance under concurrent load
    %% NOTE(Dmitry): set of Askers is finite and quite small
    case ets:lookup(hidranto_asker_counts, Asker) of
        [{Asker, X}] when X < ?SIMULTANEOUS_REQS ->
            check_asker_count(Asker, Triplets);
        [{Asker, _}] -> busy; %% fast path (1 ETS lookup for spammers)
        [] ->
            ets:insert(hidranto_asker_counts, {Asker, 0}),
            check_asker_count(Asker, Triplets)
    end.

%% NOTE(Dmitry): 2 stages because update_counter can't tell us if
%%               an entry is missing in ETS (it just throws badarg)
%%               and I don't want to catch all badargs - I don't know
%%               the exact reason of one, and lookup+insert isn't
%%               atomic in concurrent environment (therefore update_counter)
check_asker_count(Asker, Triplets) ->
    case ets:update_counter(hidranto_asker_counts, Asker, {2, 1}) of
        X when X < ?SIMULTANEOUS_REQS ->
            precheck_asker_rate(Asker, Triplets);
        _ -> ets:update_counter(hidranto_asker_counts, Asker, {2, -1}),
             busy
    end.

precheck_asker_rate(Asker, Triplets) ->
    case ets:lookup(hidranto_asker_rates, Asker) of
        [{Asker, LastAskTime, LastRate}] ->
            check_asker_rate(Asker, Triplets, LastAskTime, LastRate);
        [] ->
            %% TODO(Dmitry): GC this table!
            ets:insert(hidranto_asker_rates, {Asker, os:timestamp(), 0.0}),
            allow_ask(Asker, Triplets)
    end.

check_asker_rate(Asker, Triplets, LastAskTime, LastRate) ->
    %% FIXME(Dmitry): write this
    allow_ask(Asker, Triplets).

allow_ask(Asker, Triplets) ->
    Ref = erlang:make_ref(),
    %% TODO(Dmitry): GC this table!
    ets:insert(hidranto_refs_to_askers, {Ref, Asker,
                                         os:timestamp()}),
    {ok, Ref}.

done(Ref) ->
    [{Ref, Asker, _}] = ets:lookup(hidranto_refs_to_askers, Ref),
    ets:update_counter(hidranto_asker_counts, Asker, {2, -1}),
    ets:delete(hidranto_refs_to_askers, Ref),
    ok.

configure(_Conf) ->
    _Conf = [{queue_len, 10},
             {queue_wait_time, {10, seconds}},
             {user_limiter, [{count, 5},
                             {freq, 1}]},
             {limiters, [{user, [{count, 3},
                                 {freq, 60}]}]}], %% 60 per minute
    ok.

%%% gen_server callbacks
init([]) ->
    ets:new(hidranto_asker_counts, [public, named_table]),
    ets:new(hidranto_asker_rates, [public, named_table]),
    ets:new(hidranto_refs_to_askers, [public, named_table]),
    ets:new(asker_queues, [public, named_table,
                           {write_concurrency, true}]),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal functions

%% Asker -> {ok, Ref} | busy | timeout
regulate_asker(_Asker) ->
    %% Ref = erlang:make_ref(),
    %% case rater_try(asker, Asker) of
    %%     ok -> {ok, Ref};
    %%     wait -> case enqueue(Asker, Ref) of
    %%                 enqueued



    {ok, erlang:make_ref()}.

%% Triplets -> {ok, Ref} | busy | timeout
regulate_general(Ref, _Triplets) ->
    {ok, Ref}.
