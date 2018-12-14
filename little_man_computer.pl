/*
Matr: 830694
Written by: Gianluca Giudice.
*/

%%%% -*- Mode: Prolog -*-



%%% Include files
:- consult(compiler).
:- consult(executer).



%%% lmc_load/2: Given a file, return the content of the memory.
lmc_load(Filename, Mem) :-
    % Read the file
    open(Filename, read, Input),
    read_string(Input, _, OutputString),
    close(Input),
    % Convert ouput file string to lowercase
    string_lower(OutputString, OutputStringLower),
    % Split output string into a list of rows (Unix, Windows and MacOs support)
    split_string(OutputStringLower, '\n\r', '\n', LineList),
    % Convert assembly programm into machine code starting from line 0
    assembler(LineList, 0, MemUnresolved),
    % Resolve all undefined label
    findall(X, defined_label(X, _), [_ | DefinedLabelList]),
    resolve_labels(DefinedLabelList, MemUnresolved, MemResolved),
    % Reset labels used for compile process
    reset_labels(),
    % Compiled succesfully
    writeln('Msg: Compiled succesfully.'),
    % Fill the memory with 0s
    length(MemResolved, X),
    fill_memory(MemResolved, X, Mem), !.
lmc_load(_, _) :-
    % If compile fail, reset labels used for compile process and fail
    reset_labels(), fail.



%%% lmc_run/3
lmc_run(Filename, Input, Out) :-
    lmc_load(Filename, Mem),
    State =.. [state, 0, 0, Mem, Input, Out, noflag],
    execution_loop(State, Out).



%%% execution_loop/2
execution_loop(State, _) :-
    one_instruction(State, NewState),
    arg(5, NewState, NewOut),
    execution_loop(NewState, NewOut), !.
execution_loop(State, Out) :-
    arg(5, State, Out),
    writeln("Msg: Execution has been completed.").
    %writeln(Out).



%%% one_instruction/2: Given a state return the new State
one_instruction(State, NewState) :-
    % If is a "state" go on
    functor(State, state, 6), !,
    % Fetch
    arg(4, State, MemList),
    arg(2, State, Pc),
    nth0(Pc, MemList, Mem),
    % Decode
    OpCode is div(Mem, 100),
    Arg is mod(Mem, 100),
    % Execute
    execute_instruction(OpCode, Arg, State, NewState), !.