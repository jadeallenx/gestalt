-module(gestalt_mgr).
-behaviour(gen_server).

% public API
-export([
    start_link/4,
    start_link/5,
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

-define(TICK, '__gestalt_tick').

-record(state, {
    ets_owner            :: pid(),
    tid                  :: ets:tid(),
    directory            :: binary(),
    filehash             :: function(),
    interval             :: pos_integer(),
    interval_tref        :: reference(),
    keys = ordsets:new() :: ordsets:set()
}).

start_link(Pid, TableId, Directory, Interval) ->
    start_link(Pid, TableId, Directory, Interval, fun gestalt:default_filehash/1).

start_link(Pid, TableId, Directory, Interval, FileHashFun) ->
    gen_server:start_link(?MODULE, [Pid, TableId, Directory, Interval, FileHashFun], []).

%% We are simple_one_for_one so we don't need an identifier
child_spec() ->
    #{   id => undefined,
      start => {?MODULE, start_link, []}}.

init([Pid, TableId, Directory, Interval, HashFun]) ->
    link(Pid), % if the ets table owner pid dies, we should die too
    Path = filename:join(gestalt:get_env(root_storage_dir, "/tmp"), Directory),
    ok = filelib:ensure_dir(filename:join(Path, "dummy")),
    Tref = schedule_tick(Interval),
    {ok, #state{ ets_owner     = Pid,
                 tid           = TableId,
                 directory     = Path,
                 interval      = Interval,
                 interval_tref = Tref,
                 filehash      = HashFun
               }}.

handle_cast(_Cast, S) -> {noreply, S}.

handle_call(_Call, _From, S) -> {reply, diediedie, S}.

handle_info(_Info, S) -> {noreply, S}.

terminate(_Reason, _S) -> ok.

code_change(_OldVsn, S, _Extra) -> {ok, S}.


%% private
scheulde_tick(Millis) ->
    erlang:send_after(Millis, self(), ?TICK).
