-module(gestalt_mgr).
-behaviour(gen_server).

% public API
-export([
    start_link/0,
    child_spec/0
]).

% gen_server callbacks
-export([
    init/1,
    handle_cast/2,
    handle_call/3,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {
}).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

child_spec() ->
    #{   id => ?MODULE,
      start => {?MODULE, start_link, []}}.

init([]) ->
    {ok, #state{}}.

handle_cast(_Cast, S) -> {noreply, S}.

handle_call(_Call, _From, S) -> {reply, diediedie, S}.

handle_info(_Info, S) -> {noreply, S}.

terminate(_Reason, _S) -> ok.

code_change(_OldVsn, S, _Extra) -> {ok, S}.

