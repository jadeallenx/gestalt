-module(gestalt_mgr).
-behaviour(gen_server).

% public API
-export([
    queue_store_sync/2,
    queue_store/2,
    load/2,
    load_async/3,
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

queue_store_sync(Pid, Key) ->
    gen_server:call(Pid, {store, Key}).

queue_store(Pid, Key) ->
    gen_server:cast(Pid, {store, Key}).

load(Pid, Key) ->
    gen_server:call(Pid, {load, Key}).

load_async(Pid, Key, Reply) ->
    gen_server:cast(Pid, {load, Key, Reply}).

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

handle_cast({store, Key}, S = #state{ keys = K }) ->
    {noreply, S#state{ keys = ordsets:add_element(Key, K) }};

handle_cast({load, Key, Reply}, S = #state{ directory = D, filehash = F }) ->
    Path = make_filepath(D, F(Key)),
    maybe_spawn_loader(Path, Reply),
    {noreply, S};

handle_cast(_Cast, S) -> {noreply, S}.

handle_call({store, Key}, _From, S = #state{ keys = K }) ->
    {reply, ok, S#state{ keys = ordsets:add_element(Key, K) }};

handle_call({load, Key}, _From, S = #state{ directory = D, filehash = F }) ->
    Path = make_filepath(D, F(Key)),
    Answer = gestalt:load(Path),
    {reply, Answer, S};

handle_call(_Call, _From, S) -> {reply, diediedie, S}.

handle_info(?TICK, S = #state{ directory = D, filehash = F, tid = T,
                               keys = K, interval = I }) ->
    spawn( fun() -> store_keys(T, D, F, ordsets:to_list(K)) end ),
    Tref = schedule_tick(I),
    {noreply, S#state{ interval_tref = Tref }};

handle_info(_Info, S) -> {noreply, S}.

terminate(_Reason, _S) -> ok.

code_change(_OldVsn, S, _Extra) -> {ok, S}.

%% private
schedule_tick(Millis) ->
    erlang:send_after(Millis, self(), ?TICK).

make_filepath(D, Fn) -> filename:join(D, Fn).

maybe_spawn_loader(Path, Reply) ->
    Exists = filelib:is_regular(Path),
    maybe_spawn_loader(Exists, Path, Reply).

maybe_spawn_loader(true, Path, Reply) ->
    spawn( fun() ->
                Answer = gestalt:load(Path),
                Reply ! {gestalt_load, Answer}
           end );

maybe_spawn_loader(false, _Path, Reply) ->
    Reply ! not_found.

store_keys(_Tid, _D, _F, []) -> ok;
store_keys(Tid, D, F, [H|T]) ->
    Data = get_data(Tid, H),
    Path = make_filepath(D, F(H)),
    gestalt:store(Path, Data),
    store_keys(Tid, D, F, T).

get_data(Tid, K) ->
    case ets:lookup(Tid, K) of
        [] -> {error, {key_not_found, K}};
        [R] -> R
    end.
