-module(prop_persistent_term).
-include_lib("proper/include/proper.hrl").

%% Macros.
-define(MODEL, persistent_term_model).
-define(SHIM, persistent_term_shim).
-define(STATEM, persistent_term_statem).

%%%%%%%%%%%%%%%%%%
%%% PROPERTIES %%%
%%%%%%%%%%%%%%%%%%
prop_statem() ->
    _ShimState = ?SHIM:backup(),
    persistent_term_stress:start_link(),
    ?FORALL(Cmds, commands(?STATEM),
            begin
                % ShimState = ?SHIM:backup(),
                {History, State, Result} = run_commands(?STATEM, Cmds),
                PTState = persistent_term:get(),
                % ok = ?SHIM:restore(ShimState),
                ?WHENFAIL(print_failure_report(Cmds, State, Result, History, PTState), aggregate(command_names(Cmds), Result =:= ok))
            end).

prop_parallel_statem() ->
    _ShimState = ?SHIM:backup(),
    ?FORALL(Cmds, parallel_commands(?STATEM),
            begin
                % ShimState = ?SHIM:backup(),
                persistent_term_stress:start_link(),
                {History, State, Result} = run_parallel_commands(?STATEM, Cmds),
                PTState = persistent_term:get(),
                % ok = ?SHIM:restore(ShimState),
                ?WHENFAIL(print_failure_report(Cmds, State, Result, History, PTState), aggregate(command_names(Cmds), Result =:= ok))
            end).

%% @private
print_failure_report(Commands, State, Result, History, PTState) ->
    io:format(<<
        "=======~n",
        "Failing command sequence:~n~p~n",
        "At state: ~p~n",
        "=======~n",
        "Result: ~p~n",
        "History: ~p~n",
        "PTState: ~p~n"
    >>, [
        Commands,
        State,
        Result,
        History,
        PTState
    ]).
