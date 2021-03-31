-module(persistent_term_statem).
-behaviour(proper_statem).

-include_lib("proper/include/proper.hrl").

%% proper_statem callbacks
-export([initial_state/0]).
-export([command/1]).
-export([precondition/2]).
-export([postcondition/3]).
-export([next_state/3]).

%% Macros.
-define(MODEL, persistent_term_model).
-define(SHIM, persistent_term_shim).

initial_state() ->
    ?MODEL:new().

command(SymbolicState) when map_size(SymbolicState) =:= 0 ->
    oneof([
        % {call, ?SHIM, erase_unknown_key, [term()]},
        % {call, ?SHIM, get_empty, []},
        % {call, ?SHIM, get_unknown_key, [term()]},
        % {call, ?SHIM, get_unknown_key_default, [term(), term()]},
        {call, ?SHIM, put_non_existing, [term(), term()]}
    ]);
command(SymbolicState) ->
    Keys = maps:keys(SymbolicState),
    oneof([
        {call, ?SHIM, erase_known_key, [oneof(Keys)]},
        % {call, ?SHIM, get_non_empty, []},
        {call, ?SHIM, get_known_key, [oneof(Keys)]},
        % {call, ?SHIM, get_known_key_default, [oneof(Keys), term()]},
        % {call, ?SHIM, get_unknown_key_default, [term(), term()]},
        {call, ?SHIM, put_existing, [oneof(Keys), term()]},
        {call, ?SHIM, put_non_existing, [term(), term()]}
    ]).

precondition(State, SymbolicCall) ->
    case SymbolicCall of
        {call, _, erase_unknown_key, [_Key]} ->
            % skip for now
            false;
        {call, _, erase_known_key, [_Key]} ->
            true;
        {call, _, get_empty, []} ->
            map_size(State) =:= 0;
        {call, _, get_non_empty, []} ->
            map_size(State) =/= 0;
        {call, _, get_known_key, [Key]} ->
            maps:is_key(Key, State);
        {call, _, get_unknown_key, [Key]} ->
            not maps:is_key(Key, State);
        {call, _, get_known_key_default, [Key, _Default]} ->
            maps:is_key(Key, State);
        {call, _, get_unknown_key_default, [Key, _Default]} ->
            not maps:is_key(Key, State);
        {call, _, put_existing, [Key, _Value]} ->
            maps:is_key(Key, State);
        {call, _, put_non_existing, [Key, _Value]} ->
            not maps:is_key(Key, State)
    end.

postcondition(State, SymbolicCall, Result) ->
    case SymbolicCall of
        {call, _, erase_unknown_key, [Key]} ->
            Result =:= element(2, ?MODEL:erase(State, Key));
        {call, _, erase_known_key, [Key]} ->
            Result =:= element(2, ?MODEL:erase(State, Key));
        {call, _, get_empty, []} ->
            Result =:= element(2, ?MODEL:get(State));
        {call, _, get_non_empty, []} ->
            Result =:= element(2, ?MODEL:get(State));
        {call, _, get_known_key, [Key]} ->
            Result =:= element(2, ?MODEL:get(State, Key));
        {call, _, get_unknown_key, [Key]} ->
            Result =:= element(2, ?MODEL:get(State, Key));
        {call, _, get_known_key_default, [Key, Default]} ->
            Result =:= element(2, ?MODEL:get(State, Key, Default));
        {call, _, get_unknown_key_default, [Key, Default]} ->
            Result =:= element(2, ?MODEL:get(State, Key, Default));
        {call, _, put_existing, [_Key, _Default]} ->
            true;
        {call, _, put_non_existing, [_Key, _Default]} ->
            true
    end.

next_state(State, _Result, SymbolicCall) ->
    case SymbolicCall of
        {call, _, erase_unknown_key, [Key]} ->
            {NewState, _} = ?MODEL:erase(State, Key),
            NewState;
        {call, _, erase_known_key, [Key]} ->
            {NewState, _} = ?MODEL:erase(State, Key),
            NewState;
        {call, _, get_empty, []} ->
            {NewState, _} = ?MODEL:get(State),
            NewState;
        {call, _, get_non_empty, []} ->
            {NewState, _} = ?MODEL:get(State),
            NewState;
        {call, _, get_known_key, [Key]} ->
            {NewState, _} = ?MODEL:get(State, Key),
            NewState;
        {call, _, get_unknown_key, [Key]} ->
            {NewState, _} = ?MODEL:get(State, Key),
            NewState;
        {call, _, get_known_key_default, [Key, Default]} ->
            {NewState, _} = ?MODEL:get(State, Key, Default),
            NewState;
        {call, _, get_unknown_key_default, [Key, Default]} ->
            {NewState, _} = ?MODEL:get(State, Key, Default),
            NewState;
        {call, _, put_existing, [Key, Value]} ->
            {NewState, _} = ?MODEL:put(State, Key, Value),
            NewState;
        {call, _, put_non_existing, [Key, Value]} ->
            {NewState, _} = ?MODEL:put(State, Key, Value),
            NewState
    end.
