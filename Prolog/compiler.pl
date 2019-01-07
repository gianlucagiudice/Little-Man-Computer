/*
Matr: 830694
Written by: Gianluca Giudice.
*/

%%%% -*- Mode: Prolog -*-



%%% List of instruction and their machineCode
instruction(add, 100, _).
instruction(sub, 200, _).
instruction(sta, 300, _).
instruction(lda, 500, _).
instruction(bra, 600, _).
instruction(brz, 700, _).
instruction(brp, 800, _).
instruction(dat, 0  , _).
instruction(dat, 0     ).
instruction(hlt, 0     ).
instruction(inp, 901   ).
instruction(out, 902   ).



%%% word_reserved/1: Unify if a word is reserved.
word_reserverd(Word) :-
    atom_string(AtomWord, Word), instruction(AtomWord, _, _), !.
word_reserverd(Word) :-
    atom_string(AtomWord, Word), instruction(AtomWord, _).



%%% compile_instruction/4_2: Convert an instruction to machine code
%% "dat" is a special instruction
compile_instruction(dat, MaC, _, X) :-
    !, number_string(X, MaC), X =< 999, X >= 0.
%% Instructions with numeric arguments
compile_instruction(Instruction, Arg, _, MaC) :-
    %% If argument is a number then isn't a label
    number_string(Atom, Arg), !,
    %% Check if argument is valid
    Atom =< 99, Atom >= 0,
    %% Get the instruction opcode
    instruction(Instruction, OpCode, _),
    MaC is OpCode + Atom.
%% Instruction with label as argument
compile_instruction(Instruction, Arg, MemPointer, MaC) :-
    %% If argument is a label, evaluate the label
    evaluate_label(Arg, MemPointer, undefined_label),
    %% Then compile the instruction
    compile_instruction(Instruction, "0", _, MaC), !.
%% Instructions whitout arguments
compile_instruction(dat, 0) :- !.
compile_instruction(Instruction, Mac) :- instruction(Instruction, Mac).



%%% assert_labels/0: Assert labels used as placeholder
assert_labels() :-
    %% Assert placeholder labels
    assert(defined_label('', '')),
    assert(undefined_label('', '')).
%%% reset_labels/0: Reset label to initial state
reset_labels() :-
    %% Retract labels used at compile time
    retractall(defined_label(_, _)),
    retractall(undefined_label(_, _)).



%%% assembler/4: Convert whole assembly program into machine code.
assembler([], _, []).
%% Memory overflow
assembler(_, MemPointer, _) :-
    MemPointer >= 100,
    writeln('COMPILE ERROR: Too many instructions to load in memory.'), !, fail.
%% Skip blank line
assembler([Line | RestFile], MemPointer, Mem) :-
    split_assembly_line(Line, [HeadSplittedLine | _]),
    %% If Line is blank skip it
    HeadSplittedLine = "", !,
	assembler(RestFile, MemPointer, Mem).
%% Convert each line into machine code
assembler([Line | RestFile], MemPointer, [MemLine | Mem]) :-
    split_assembly_line(Line, SplittedLine),
    %% Convert single line into machine code
    assembler_line(SplittedLine, MemPointer, MemLine), !,
    NewMemPointer is MemPointer + 1,
    assembler(RestFile, NewMemPointer, Mem).
%% If not unify with other, then is a compile error
assembler([Line | _], _, _) :-
    format('Instruction: "~s".\n', [Line]), fail.



%%% split_assembly_line/2: split assembly line into a list of single word
split_assembly_line(Line, SplittedLine) :-
    %% Remove comment in line.
    string_codes(Line, Codes),
    remove_comment(Codes, NoComment),
    string_codes(InstructionNoComment, NoComment),
    %% Split into a list of instruction
    split_string(InstructionNoComment, " \t", " \t", SplittedLine).
%%% remove_comment/2: remove comment from a line
remove_comment([], []).
remove_comment([CommentChar, CommentChar | _], []) :-
    string_codes("/", [CommentChar | _]), !.
remove_comment([T | Ts], [T | Rest]) :- remove_comment(Ts, Rest).




%%% assembler_line/3: Convert a single instruction into machine code.
%% Len = 1
assembler_line([Instruction], _, MachineCode) :-
    atom_string(AtomInstruction, Instruction),
    compile_instruction(AtomInstruction, MachineCode), !.
%% Len = 2
assembler_line([X, Y], _, _) :-
    word_reserverd(X), word_reserverd(Y), !,
    writeln('COMPILE ERROR: Invalid instruction'), fail.
assembler_line([Label, Instruction], MemPointer, MachineCode) :-
    word_reserverd(Instruction), !,
    evaluate_label(Label, MemPointer, defined_label),
    assembler_line([Instruction], _, MachineCode).
assembler_line([Instruction, Argument], MemPointer, MachineCode) :-
    word_reserverd(Instruction),
    atom_string(AtomInstruction, Instruction),
    compile_instruction(AtomInstruction, Argument, MemPointer, MachineCode), !.
%% Len = 3
assembler_line([X, _, _], _, _) :-
    word_reserverd(X), !,
    writeln('COMPILE ERROR: Invalid instruction'), fail.
assembler_line([Label, Instruction, Argument], MemPointer, MachineCode) :-
    word_reserverd(Instruction), !,
    assembler_line([Instruction, Argument], MemPointer, MachineCode),
    evaluate_label(Label, MemPointer, defined_label), !.
%% If not unify with other, then is an invalid instruction
assembler_line(_, _, _) :-
    writeln('COMPILE ERROR: Invalid instruction'), fail.



%%% evaluate_label/3: Add a label to knowledge base
%% Label is a reserved word
evaluate_label(Label, _, _) :-
    word_reserverd(Label), !, fail.
%% Label alredy defined
evaluate_label(Label, _, defined_label) :-
    atom_string(AtomLabel, Label),
    %% Label can not be defined more than once
    defined_label(AtomLabel, _), !,
    format('COMPILE ERROR: Label "~s" is alredy defined.\n', [Label]), fail.
%% Add label to knowledge base
evaluate_label(Label, MemPointer, Type) :-
    atom_string(AtomLabel, Label),
    %% Create assertz = assertz(labelType(AtomLabel, MemPointer))
    %% Create the functor to add in knowledge base
    functor(Func, Type, 2), arg(1, Func, AtomLabel), arg(2, Func, MemPointer),
    %% Assert the functor
    functor(Assert, assertz, 1), arg(1, Assert, Func), call(Assert).



%%% resolve_labels/2: Convert a label to corresponding value
%% All labels resolved succesfully
resolve_labels([], Mem, Mem) :-
    %% If the only undefined_label is placeholder, then all labels are resolved
    findall(X, undefined_label(X, _), LabelName),
    length(LabelName, 1), !.
%% If still some labels are undefined
resolve_labels([], Mem, Mem) :-
    findall(X, undefined_label(X, _), [_ | LabelsName]),
    writeln('COMPILE ERROR: Some labels undefined:'),
    writeln(LabelsName), fail.
%% Resolve recursivly labels
resolve_labels([LabelName | List], MemUnresolved, Mem) :-
    findall(X, undefined_label(LabelName, X), Rows),
    defined_label(LabelName, LabelValue),
    resolve_one_label(Rows, LabelName, LabelValue, MemUnresolved, MemNew),
    %% Recursivly resolve the rest of the label
    resolve_labels(List, MemNew, Mem).
%%% resolve_one_label/2: Resolve all values relative to a single label
resolve_one_label([], _, _, Mem, Mem).
resolve_one_label([Row | List], LabelName, LabelValue, MemUnresolved, Mem) :-
    nth0(Row, MemUnresolved, OldValue, Rest),
    NewValue is OldValue + LabelValue,
    nth0(Row, OldMem, NewValue, Rest),
    %% Remove resolved label form knowledge base
    retract(undefined_label(LabelName, Row)),
    %% Recursivly resolve the label
    resolve_one_label(List, LabelName, LabelValue, OldMem, Mem).



%%% fill-memory/2: Fill the memory with 0s
fill_memory([], 100, []).
fill_memory([], OldNumber, [0 | M]) :-
    NewNumber is OldNumber + 1,
    fill_memory([], NewNumber, M), !.
fill_memory([M | Mem], N, [M | FilledMem]) :-
    fill_memory(Mem, N, FilledMem).