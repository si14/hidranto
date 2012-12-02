-module(hidranto).
-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-compile([export_all]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%% API

start() ->
    application:start(hidranto).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

ask(Asker, Triplets) ->
    case regulate_asker(Asker) of
        {ok, Ref} -> regulate_general(Ref, Triplets);
        Error -> Error
    end.

done(_Ref) ->
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
regulate_asker(Asker) ->
    %% Ref = erlang:make_ref(),
    %% case rater_try(asker, Asker) of
    %%     ok -> {ok, Ref};
    %%     wait -> case enqueue(Asker, Ref) of
    %%                 enqueued



    {ok, erlang:make_ref()}.

%% Triplets -> {ok, Ref} | busy | timeout
regulate_general(Ref, _Triplets) ->
    {ok, Ref}.
