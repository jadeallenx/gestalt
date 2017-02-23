-module(gestalt).

-export([
    start/0,
    get_env/2,
    get_env/1,
    default_filehash/1,
    to_hex/1,
    load/1,
    store/2
]).

start() ->
    application:ensure_all_started(gestalt).

get_env(Key, Default) ->
    application:get_env(gestalt, Key, Default).

get_env(Key) ->
    get_env(Key, undefined).

default_filehash(Term) ->
    to_hex(erlang:md5(term_to_binary(Term))).

%% slightly adapted from
%% http://stackoverflow.com/questions/3768197/erlang-ioformatting-a-binary-to-hex
to_hex(Bin) when is_binary(Bin) ->
    << <<(hex(H)),(hex(L))>> || <<H:4,L:4>> <= Bin >>.

hex(C) when C < 10 -> $0 + C;
hex(C) -> $a + C - 10.

load(Path) ->
    maybe_load(filelib:is_regular(Path), Path).

maybe_load(true, Path) ->
    {ok, Data} = file:read_file(Path),
    binary_to_term(Data);

maybe_load(false, _Path) ->
    not_found.

store(Path, Data) ->
    file:write_file(Path, term_to_binary(Data)).
