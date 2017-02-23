-module(gestalt_sup).
-behaviour(supervisor).

-export([
    start_link/0,
    start_new_manager/3,
    start_new_manager/4,
    start_new_manager/5
]).

-export([
    init/1
]).

-define(SERVER, ?MODULE).
-define(DEFAULT_INTERVAL, 5000).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_new_manager(TableOwnerPid, TableId, Directory) ->
    start_new_manager(TableOwnerPid, TableId, Directory, gestalt:get_env(default_interval, ?DEFAULT_INTERVAL)).

start_new_manager(TableOwnerPid, TableId, Directory, Interval) ->
    supervisor:start_child(?SERVER, [TableOwnerPid, TableId, Directory, Interval]).

start_new_manager(TableOwnerPid, TableId, Directory, Interval, HashFun) ->
    supervisor:start_child(?SERVER, [TableOwnerPid, TableId, Directory, Interval, HashFun]).

init([]) ->
    Flags = #{strategy  => simple_one_for_one,
              intensity => 10,
              period    => 5},
    {ok, {Flags, [ gestalt_mgr:child_spec() ]}}.
