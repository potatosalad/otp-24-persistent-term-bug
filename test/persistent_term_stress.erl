-module(persistent_term_stress).

-export([all_atoms/0]).
-export([random_atom/0]).

-export([start_link/0]).
-export([sup_init/1]).
-export([wrk_init/1]).

all_atoms() ->
    atoms_starting_at(0).

atom_by_number(N) ->
    binary_to_term(<<131,75,N:24>>).

atoms_starting_at(N) ->
    try atom_by_number(N) of
        Atom ->
            [Atom] ++ atoms_starting_at(N + 1)
    catch
        error:badarg ->
            []
    end.

random_atom() ->
    AllAtoms = all_atoms(),
    lists:nth(rand:uniform(length(AllAtoms)), AllAtoms).

start_link() ->
    {ok, Pid} = proc_lib:start_link(?MODULE, sup_init, [erlang:self()]),
    true = erlang:link(Pid),
    {ok, Pid}.

sup_init(Parent) ->
    case erlang:whereis(?MODULE) of
        ExistingPid when is_pid(ExistingPid) ->
            ok = proc_lib:init_ack(Parent, {ok, ExistingPid}),
            erlang:exit(normal);
        undefined ->
            try erlang:register(?MODULE, self()) of
                true ->
                    _ = erlang:process_flag(trap_exit, true),
                    ok = proc_lib:init_ack(Parent, {ok, erlang:self()}),
                    sup_loop(#{})
            catch
                _:_:_ ->
                    sup_init(Parent)
            end
    end.

sup_loop(Pids) ->
    receive
        Msg ->
            io:format("[sup_loop] unhandled message: ~p~n", [Msg]),
            sup_loop(Pids)
    after 1000 ->
        {ok, Pid} = proc_lib:start_link(?MODULE, wrk_init, [erlang:self()]),
        sup_loop(maps:put(Pid, [], Pids))
        % NodeWeights = [{Node, Weight} || {{node_weight, Node}, Weight} <- persistent_term:get()],
        % io:format("NodeWeights = ~p~n", [NodeWeights]),
        % sup_loop(Pids)
    end.

wrk_init(Parent) ->
    ok = proc_lib:init_ack(Parent, {ok, erlang:self()}),
    wrk_loop(random_atom()).

wrk_loop(State) ->
    receive
        Msg ->
            io:format("[wrk_loop:~p] unhandled message: ~p~n", [State, Msg]),
            wrk_loop(State)
    after 100 ->
        persistent_term:put({node_weight, State}, persistent_term:get({node_weight, State}, 100) + 1),
        NodeWeights = [{Node, Weight} || {{node_weight, Node}, Weight} <- persistent_term:get()],
        ok = wrk_reset(State, NodeWeights),
        % NodeWeights2 = [{Node, Weight} || {{node_weight, Node}, Weight} <- persistent_term:get()],
        % io:format("[wrk_loop:~p] NodeWeights = ~p~n", [State, NodeWeights2]),
        wrk_loop(State)
    end.

wrk_reset(State, [{State, _Weight} | Rest]) ->
    wrk_reset(State, Rest);
wrk_reset(State, [{Node, Weight} | Rest]) ->
    case rand:uniform(100) of
        1 ->
            persistent_term:erase({node_weight, Node});
        _ ->
            persistent_term:put({node_weight, Node}, Weight - 100)
    end,
    wrk_reset(State, Rest);
wrk_reset(_State, []) ->
    ok.
