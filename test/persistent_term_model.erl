-module(persistent_term_model).

-export([new/0]).
-export([erase/2]).
-export([get/1]).
-export([get/2]).
-export([get/3]).
-export([info/1]).
-export([put/3]).

new() ->
    maps:from_list([{Key, Value} || {Key, Value} <- persistent_term:get(), not (is_tuple(Key) andalso element(1, Key) =:= node_weight)]).
    % maps:new().

erase(State, Key) ->
    case maps:is_key(Key, State) of
        true ->
            NewState = maps:remove(Key, State),
            {NewState, true};
        false ->
            {State, false}
    end.

get(State) ->
    % {State, maps:to_list(State)}.
    {State, State}.

get(State, Key) ->
    {State, maps:get(Key, State)}.

get(State, Key, Default) ->
    {State, maps:get(Key, State, Default)}.

info(State) ->
    {State, #{count => maps:size(State), memory => erlang:external_size(maps:to_list(State))}}.

put(State, Key, Value) ->
    NewState = maps:put(Key, Value, State),
    {NewState, ok}.
