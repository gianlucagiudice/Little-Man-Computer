%%%% Matr: 830694
%%%% Written by: Gianluca Giudice.

%%%% -*- Mode: Prolog -*-



%%% execute_instruction/4: Execute a single instruction
%% add
execute_instruction(1, Arg, State, NewState) :-
    State =.. [StateType, Acc, PC, Mem, Input, Out, _],
    %% Get target memory cell
    nth0(Arg, Mem, Cell),
    %% Execute add
    EvaluateAcc is Acc + Cell,
    %% Set the flag
    evaluate_flag(EvaluateAcc, NewFlag),
    NewAcc is mod(EvaluateAcc, 1000),
    %% Increment the PC
    increment_pc(PC, NewPC),
    %% Create the new state
    NewState =.. [StateType, NewAcc, NewPC, Mem, Input, Out, NewFlag].
%% sub
execute_instruction(2, Arg, State, NewState) :-
    State =.. [StateType, Acc, PC, Mem, Input, Out, _],
    %% Get target memory cell
    nth0(Arg, Mem, Cell),
    %% Execute sub
    EvaluateAcc is Acc - Cell,
    %% Set the flag
    evaluate_flag(EvaluateAcc, NewFlag),
    NewAcc is mod(EvaluateAcc, 1000),
    %% Increment the PC
    increment_pc(PC, NewPC),
    %% Create the new state
    NewState =.. [StateType, NewAcc, NewPC, Mem, Input, Out, NewFlag].
%% sta
execute_instruction(3, Arg, State, NewState) :-
    State =.. [StateType, Acc, PC, Mem, Input, Out, Flag],
    %% Store the Acc value in memory
    replace_pos(Mem, Arg, Acc, NewMem),
    %% Increment the PC
    increment_pc(PC, NewPC),
    %% Create the new state
    NewState =.. [StateType, Acc, NewPC, NewMem, Input, Out, Flag].
%% lda
execute_instruction(5, Arg, State, NewState) :-
    State =.. [StateType, _, PC, Mem, Input, Out, Flag],
    %% Load Acc from Memory
    nth0(Arg, Mem, NewAcc),
    %% Increment the PC
    increment_pc(PC, NewPC),
    %% Create the new state
    NewState =.. [StateType, NewAcc, NewPC, Mem, Input, Out, Flag].
%% bra
execute_instruction(6, Arg, State, NewState) :-
    State =.. [StateType, Acc, _, Mem, Input, Out, Flag],
    %% Jump
    NewPC = Arg,
    %% Create the new state
    NewState =.. [StateType, Acc, NewPC, Mem, Input, Out, Flag].
%% brz
execute_instruction(7, Arg, State, NewState) :-
    State =.. [StateType, Acc, _, Mem, Input, Out, Flag],
    %% Brz condition
    Acc = 0, Flag = noflag, !,
    %% Jump
    NewPC = Arg,
    %% Create the new state
    NewState =.. [StateType, Acc, NewPC, Mem, Input, Out, Flag].
execute_instruction(7, _, State, NewState) :-
    State =.. [StateType, Acc, PC, Mem, Input, Out, Flag],
    %% Don't jump, just increment PC by 1
    increment_pc(PC, NewPC),
    %% Create the new state
    NewState =.. [StateType, Acc, NewPC, Mem, Input, Out, Flag].
%% brp
execute_instruction(8, Arg, State, NewState) :-
    State =.. [StateType, Acc, _, Mem, Input, Out, Flag],
    %% Brp condition
    Flag = noflag, !,
    %% Jump
    NewPC = Arg,
    %% Create the new state
    NewState =.. [StateType, Acc, NewPC, Mem, Input, Out, Flag].
execute_instruction(8, _, State, NewState) :-
    State =.. [StateType, Acc, PC, Mem, Input, Out, Flag],
    %% Don't jump, just increment PC by 1
    increment_pc(PC, NewPC),
    %% Create the new state
    NewState =.. [StateType, Acc, NewPC, Mem, Input, Out, Flag].
%% inp
execute_instruction(9, 01 , State, _) :-
    arg(4, State, Input),
    Input = [], !,
    writeln("RUNTIME ERROR: Trying to read empty input list"), fail.
execute_instruction(9, 01 , State, NewState) :-
    State =.. [StateType, _, PC, Mem, [I | Input], Out, Flag],
    %% Increment the PC
    increment_pc(PC, NewPC),
    %% Create the new state    
    NewState =.. [StateType, I, NewPC, Mem, Input, Out, Flag].
%% out
execute_instruction(9, 02 , State, NewState) :-
    State =.. [StateType, Acc, PC, Mem, Input, Out, Flag],
    %% Increment the PC
    increment_pc(PC, NewPC),
    %% Append Acc to Out list
    append_element(Acc, Out, NewOut),
    %% Create the new state    
    NewState =.. [StateType, Acc, NewPC, Mem, Input, NewOut, Flag].
%% hlt
execute_instruction(0, _, State, NewState) :-
    State =.. [_, Acc, PC, Mem, Input, Out, Flag],
    %% Change state to halted_state the new state
    NewState =.. [halted_state, Acc, PC, Mem, Input, Out, Flag].
%% error
execute_instruction(Opc, Arg, _, _) :-
    Mac is Opc * 100,
    Instruction is Mac + Arg,
    format("RUNTIME ERROR: Instruction ~w not valid.~n", [Instruction]),
    fail.



%%% evaluate_flag/2: set flag if overflow or underflow 
evaluate_flag(Acc, NewFlag) :-
    Acc >= 1000, !,
    NewFlag = flag.
evaluate_flag(Acc, NewFlag) :-
    Acc < 0, !,
    NewFlag = flag.
evaluate_flag(_, NewFlag) :-
    NewFlag = noflag.



%%% increment_pc/2: Increment programm counter
increment_pc(PC, NewPC) :-
    PCinc is PC + 1,
    NewPC is mod(PCinc, 100).



%%% replace_pos/4: Change an element in a list
replace_pos(List, Pos, Symbol, NewList) :-
    nth0(Pos, List, _, Rest),
    nth0(Pos, NewList, Symbol, Rest).



%%% append_element/3: Append an element to last element of a list
append_element(Acc, [], [Acc]).
append_element(Acc, [O | Out], [O | NewOut]) :-
    append_element(Acc, Out, NewOut).