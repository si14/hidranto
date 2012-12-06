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
-define(MIN_DELAY, 1000000).

-record(state, {}).

%%% API

start() ->
    [application:start(App) || App <- [compiler, syntax_tools, lager,
                                       gproc,
                                       hidranto]].

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

test(Asker, T) ->
    case ask(Asker) of
        {ok, Ref} ->
            io:format("Spawn with ref ~p~n", [Ref]),
            spawn(fun() -> timer:sleep(T), hidranto:done(Ref) end);
        Other -> Other
    end.

ask(Asker, Triplets) ->
    case ask(Asker) of
        {ok, AskerRef} ->
            case hidranto_queue:enqueue(Triplets) of
                {ok, QueueRef} ->
                    {ok, {AskerRef, QueueRef}};
                {error, _Reason}=Err ->
                    done(AskerRef),
                    Err
            end;
        {error, _Reason}=Err -> Err
    end.

ask(Asker) ->
    precheck_asker_count(Asker).

enqueue(AskerRef, Triplets) ->
    hidranto_queue:enqueue(Triplets).

precheck_asker_count(Asker) ->
    %% NOTE(Dmitry): maybe it's better to skip lookup to impore ETS
    %% performance under concurrent load
    %% NOTE(Dmitry): set of Askers is finite and quite small
    case ets:lookup(hidranto_asker_counts, Asker) of
        [{Asker, X}] when X < ?SIMULTANEOUS_REQS ->
            check_asker_count(Asker);
        %% fast path (1 ETS lookup for spammers)
        [{Asker, _}] -> {error, too_much};
        [] ->
            ets:insert(hidranto_asker_counts, {Asker, 0}),
            check_asker_count(Asker)
    end.

%% NOTE(Dmitry): 2 stages because update_counter can't tell us if
%%               an entry is missing in ETS (it just throws badarg)
%%               and I don't want to catch all badargs - I don't know
%%               the exact reason of one, and lookup+insert isn't
%%               atomic in concurrent environment (therefore update_counter)
check_asker_count(Asker) ->
    case ets:update_counter(hidranto_asker_counts, Asker, 1) of
        X when X < ?SIMULTANEOUS_REQS ->
            check_asker_rate(Asker);
        _ -> deny_ask(Asker, too_much)
    end.

check_asker_rate(Asker) ->
    case ets:lookup(hidranto_asker_rates, Asker) of
        [{Asker, LastAskTime, MinDelay}] ->
            Now = os:timestamp(),
            case time_diff(LastAskTime, Now) > MinDelay of
                false -> deny_ask(Asker, too_often);
                true ->
                    ets:insert(hidranto_asker_rates, {Asker, Now, MinDelay}),
                    allow_ask(Asker)
            end;
        [] ->
            %% TODO(Dmitry): GC this table!
            ets:insert(hidranto_asker_rates,
                       {Asker, os:timestamp(), ?MIN_DELAY}),
            allow_ask(Asker)
    end.

allow_ask(Asker) ->
    Ref = erlang:make_ref(),
    %% TODO(Dmitry): GC this table!
    ets:insert(hidranto_refs_to_askers, {Ref, Asker,
                                         os:timestamp()}),
    {ok, Ref}.

deny_ask(Asker, Reason) ->
    ets:update_counter(hidranto_asker_counts, Asker, -1),
    {error, Reason}.

time_diff({Mega1, S1, Micro1}, {Mega2, S2, Micro2}) ->
    abs(Mega2 - Mega1) * 1000 * 1000 * 1000 * 1000 +
        abs(S2 - S1) * 1000 * 1000 +
        abs(Micro2 - Micro1).

done(Ref) ->
    io:format("Done: ~p~n", [Ref]),
    [{Ref, Asker, _}] = ets:lookup(hidranto_refs_to_askers, Ref),
    K = ets:update_counter(hidranto_asker_counts, Asker, -1),
    io:format("Counter: ~p~n", [K]),
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
    ets:new(hidranto_config, [public, named_table,
                              {read_concurrency, true}]),
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
