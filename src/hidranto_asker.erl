-module(hidranto_asker).
-behaviour(gen_server).
-include("hidranto.hrl").

%% API
-export([start_link/0]).
-export([ask/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(ASKER_WORKER_NUMBER, 10).

-record(state, {}).

%%% API
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

ask(Asker) ->
    AskerWorker = case gproc:where({n, l, {asker, Asker}}) of
                      undefined -> hidranto_asker_worker:new(Asker);
                      Pid when is_pid(Pid) -> Pid
                  end,
    hidranto_asker_worker:ask(AskerWorker, Asker).

%%% gen_server callbacks
init([]) ->
    {ok, #state{}}.

handle_call(Request, _From, State) ->
    lager:warning("unknown call in ~p: ~p", [?MODULE, Request]),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    lager:warning("unknown cast in ~p: ~p", [?MODULE, Msg]),
    {noreply, State}.

handle_info(Info, State) ->
    lager:warning("unknown info in ~p: ~p", [?MODULE, Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal functions
