-module(persistent_term_shim).

-export([backup/0]).
-export([restore/1]).

%% Commands.
-export([erase_known_key/1]).
-export([erase_unknown_key/1]).
-export([get_empty/0]).
-export([get_non_empty/0]).
-export([get_known_key/1]).
-export([get_unknown_key/1]).
-export([get_known_key_default/2]).
-export([get_unknown_key_default/2]).
-export([put_existing/2]).
-export([put_non_existing/2]).

backup() ->
    State = persistent_term:get(),
    ok = do_backup(State),
    State.

restore(State) ->
    _ = backup(),
    ok = do_restore(State),
    ok.

erase_known_key(Key) ->
    persistent_term:erase(Key).

erase_unknown_key(Key) ->
    persistent_term:erase(Key).

get_empty() ->
    maps:from_list(persistent_term:get()).

get_non_empty() ->
    maps:from_list(persistent_term:get()).

get_known_key(Key) ->
    persistent_term:get(Key).

get_unknown_key(Key) ->
    persistent_term:get(Key).

get_known_key_default(Key, Default) ->
    persistent_term:get(Key, Default).

get_unknown_key_default(Key, Default) ->
    persistent_term:get(Key, Default).

put_existing(Key, Value) ->
    persistent_term:put(Key, Value).

put_non_existing(Key, Value) ->
    persistent_term:put(Key, Value).

%% @private
do_backup([{Key, _Value} | Tail]) ->
    true = persistent_term:erase(Key),
    do_backup(Tail);
do_backup([]) ->
    ok.

%% @private
do_restore([{Key, Value} | Tail]) ->
    ok = persistent_term:put(Key, Value),
    do_restore(Tail);
do_restore([]) ->
    ok.
