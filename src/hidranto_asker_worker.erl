-module(hidranto_asker_worker).
-behaviour(gen_server).
-include("hidranto.hrl").

%% API
-export([start_link/1]).
-export([new/1, ask/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-define(SHUTDOWN_TIMEOUT, 30000). %% milliseconds

-record(state, {req_count   :: integer()}).

%%% API
start_link(Asker) ->
    gen_server:start_link(?MODULE, [Asker], []).

ask(Worker, Asker) ->
    gen_server:call(Worker, {ask, Asker}, 1000).

new(Asker) ->
    {ok, Pid} = supervisor:start_child(hidranto_asker_sup, [Asker]),
    Pid.

%%% gen_server callbacks
init([Asker]) ->
    gproc:reg({n, l, {asker, Asker}}),
    {ok, #state{}, ?SHUTDOWN_TIMEOUT}.

handle_call(Request, _From, State) ->
    lager:warning("unknown call in ~p: ~p", [?MODULE, Request]),
    {reply, ok, State, ?SHUTDOWN_TIMEOUT}.

handle_cast(Msg, State) ->
    lager:warning("unknown cast in ~p: ~p", [?MODULE, Msg]),
    {noreply, State, ?SHUTDOWN_TIMEOUT}.

handle_info(timeout, #state{req_count=RC}=State) when RC > 0 ->
    {noreply, State};
handle_info(timeout, State) ->
    {stop, {shutdown, timeout}, State};
handle_info(Info, State) ->
    lager:warning("unknown info in ~p: ~p", [?MODULE, Info]),
    {noreply, State, ?SHUTDOWN_TIMEOUT}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal functions
