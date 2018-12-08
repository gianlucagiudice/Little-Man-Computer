%%%% -*- Mode: Prolog -*-

/*
little_man_computer.pl
Written by: Gianluca Giudice.
*/

/*
%%% execute_instruction/4: Execute a single instruction
execute_instruction(1, Arg, State, NewState) :-
    % add
    fail.
execute_instruction(2, Arg, State, NewState) :-
    % sub
    fail.
execute_instruction(3, Arg, State, NewState) :-
    % sta
    fail.
execute_instruction(5, Arg, State, NewState) :-
    % lda
    fail.
execute_instruction(6, Arg, State, NewState) :-
    % bra
    fail.
execute_instruction(7, Arg, State, NewState) :-
    % brz
    fail.
execute_instruction(8, Arg, State, NewState) :-
    % brp
    fail.
execute_instruction(0, _  , State, NewState) :-
    % hlt
    fail.
execute_instruction(9, 01 , State, NewState) :-
    % inp
    fail.
execute_instruction(9, 02 , State, NewState) :-
    % out
    fail.
*/

valid_arg(Argument, Atom) :-
    number_string(Atom, Argument), Atom =< 99, Atom >= 0.
%%% compile_instruction/4: Convert an instruction to machine code
% Instruction with numeric argument
compile_instruction(add, Arg, _, MaC) :-
    number_string(Atom, Arg), !, Atom =< 99, Atom >= 0, MaC is 100 + Atom.
compile_instruction(sub, Arg, _, MaC) :-
    number_string(Atom, Arg), !, Atom =< 99, Atom >= 0, MaC is 200 + Atom.
compile_instruction(sta, Arg, _, MaC) :-
    number_string(Atom, Arg), !, Atom =< 99, Atom >= 0, MaC is 300 + Atom.
compile_instruction(lda, Arg, _, MaC) :-
    number_string(Atom, Arg), !, Atom =< 99, Atom >= 0, MaC is 500 + Atom.
compile_instruction(bra, Arg, _, MaC) :-
    number_string(Atom, Arg), !, Atom =< 99, Atom >= 0, MaC is 600 + Atom.
compile_instruction(brz, Arg, _, MaC) :-
    number_string(Atom, Arg), !, Atom =< 99, Atom >= 0, MaC is 700 + Atom.
compile_instruction(brp, Arg, _, MaC) :-
    number_string(Atom, Arg), !, Atom =< 99, Atom >= 0, MaC is 800 + Atom.
% Instruction with label as argument
compile_instruction(add, Arg, MemPointer, MaC) :-
    evaluate_label(Arg, MemPointer, undefined_label),
    compile_instruction(add, "0", _, MaC).
compile_instruction(sub, Arg, MemPointer, MaC) :-
    evaluate_label(Arg, MemPointer, undefined_label),
    compile_instruction(sub, "0", _, MaC).
compile_instruction(sta, Arg, MemPointer, MaC) :-
    evaluate_label(Arg, MemPointer, undefined_label),
    compile_instruction(sta, "0", _, MaC).
compile_instruction(lda, Arg, MemPointer, MaC) :-
    evaluate_label(Arg, MemPointer, undefined_label),
    compile_instruction(lda, "0", _, MaC).
compile_instruction(bra, Arg, MemPointer, MaC) :-
    evaluate_label(Arg, MemPointer, undefined_label),
    compile_instruction(bra, "0", _, MaC).
compile_instruction(brz, Arg, MemPointer, MaC) :-
    evaluate_label(Arg, MemPointer, undefined_label),
    compile_instruction(brz, "0", _, MaC).
compile_instruction(brp, Arg, MemPointer, MaC) :-
    evaluate_label(Arg, MemPointer, undefined_label),
    compile_instruction(brp, "0", _, MaC).
compile_instruction(dat, MaC, _, X) :-
    number_string(X, MaC), X =< 999, X >= 0.
compile_instruction(dat, MaC) :- compile_instruction(dat, "0", _, MaC).
compile_instruction(hlt, 0).
compile_instruction(inp, 901).
compile_instruction(out, 902).

word_reserverd(Word) :-
    atom_string(AtomWord, Word), compile_instruction(AtomWord, "0", _, _), !.
word_reserverd(Word) :-
    atom_string(AtomWord, Word), compile_instruction(AtomWord, _).



%%% defined_label/2: Label defined in the program. (labelName, MemPointer)
defined_label('', '').      % Placeholder
%%% undefined_label/2: Label undefined in the program. (labelName, MemPointer)
undefined_label('', '').    % Placeholder



%%% assembler/4: Convert whole assembly program into machine code.
assembler([], _, []).
% Memory overflow
assembler(_, MemPointer, _) :-
    MemPointer >= 100,
    writeln('COMPILE ERROR: Too many instructions to load in memory.'), !, fail.
% Skip blank line
assembler([Line | RestFile], MemPointer, Mem) :-
    split_assembly_line(Line, [HeadSplittedLine | _]),
    % If Line is blank skip it
    HeadSplittedLine = "", !,
	assembler(RestFile, MemPointer, Mem).
% Convert each line into machine code
assembler([Line | RestFile], MemPointer, [MemLine | Mem]) :-
    split_assembly_line(Line, SplittedLine),
    % Convert single line into machine code
    assembler_line(SplittedLine, MemPointer, MemLine), !,
    NewMemPointer is MemPointer + 1,
    assembler(RestFile, NewMemPointer, Mem).
% If not unify with other, then is a compile error
assembler([Line | _], _, _) :-
    format('Instruction: "~s".\n', [Line]), fail.


%%% split_assembly_line/2: split assembly line into a list of single word
split_assembly_line(Line, SplittedLine) :-
    % Remove comment in line.
    string_codes(Line, Codes),
    remove_comment(Codes, NoComment),
    string_codes(InstructionNoComment, NoComment),
    % Split into a list of instruction
    split_string(InstructionNoComment, " \t", " \t", SplittedLine).
%%% remove_comment/2: remove comment from a line
remove_comment([], []).
remove_comment([CommentChar, CommentChar | _], []) :-
    string_codes("/", [CommentChar | _]), !.
remove_comment([T | Ts], [T | Rest]) :- remove_comment(Ts, Rest).

%%% assembler_line/3: Convert a single instruction into machine code.
% Len = 1
assembler_line([Instruction], _, MachineCode) :-
    atom_string(AtomInstruction, Instruction),
    compile_instruction(AtomInstruction, MachineCode), !.
% Len = 2
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
% Len = 3
assembler_line([X, _, _], _, _) :-
    word_reserverd(X), !,
    writeln('COMPILE ERROR: Invalid instruction'), fail.
assembler_line([Label, Instruction, Argument], MemPointer, MachineCode) :-
    word_reserverd(Instruction), !,
    assembler_line([Instruction, Argument], MemPointer, MachineCode),
    evaluate_label(Label, MemPointer, defined_label), !.
% If not unify with other, then is an invalid instruction
assembler_line(_, _, _) :-
    writeln('COMPILE ERROR: Invalid instruction'), fail.



%%% evaluate_label/3: Add a label to knowledge base
% Label is a reserved word
evaluate_label(Label, _, _) :-
    word_reserverd(Label), !, fail.
% Label alredy defined
evaluate_label(Label, _, defined_label) :-
    atom_string(AtomLabel, Label),
    % Label can not be defined more than once
    defined_label(AtomLabel, _), !,
    format('COMPILE ERROR: Label "~s" is alredy defined.\n', [Label]), fail.
% Add label to knowledge base
evaluate_label(Label, MemPointer, Type) :-
    atom_string(AtomLabel, Label),
    % Create assertz = assertz(labelType(AtomLabel, MemPointer))
    % Create the functor to add in knowledge base
    functor(Func, Type, 2), arg(1, Func, AtomLabel), arg(2, Func, MemPointer),
    % Assert the functor
    functor(Assert, assertz, 1), arg(1, Assert, Func), call(Assert).



%%% resolve_labels/2: Convert a label to corresponding value
% All labels resolved succesfully
resolve_labels([], Mem, Mem) :-
    % If the only undefined_label is placeholder, then all labels are resolved
    findall(X, undefined_label(X, _), LabelName),
    length(LabelName, 1), !.
% If still some labels are undefined
resolve_labels([], Mem, Mem) :-
    findall(X, undefined_label(X, _), [_ | LabelsName]),
    writeln('COMPILE ERROR: Some labels undefined:'),
    writeln(LabelsName), fail.
% Resolve recursivly labels
resolve_labels([LabelName | List], MemUnresolved, Mem) :-
    findall(X, undefined_label(LabelName, X), Rows),
    defined_label(LabelName, LabelValue),
    resolve_one_label(Rows, LabelName, LabelValue, MemUnresolved, MemNew),
    % Recursivly resolve the rest of the label
    resolve_labels(List, MemNew, Mem).
%%% resolve_one_label/2: Resolve all values relative to a single label
resolve_one_label([], _, _, Mem, Mem).
resolve_one_label([Row | List], LabelName, LabelValue, MemUnresolved, Mem) :-
    nth0(Row, MemUnresolved, OldValue, Rest),
    NewValue is OldValue + LabelValue,
    nth0(Row, OldMem, NewValue, Rest),
    % Remove resolved label form knowledge base
    retract(undefined_label(LabelName, Row)),
    % Recursivly resolve the label
    resolve_one_label(List, LabelName, LabelValue, OldMem, Mem).



% fill-memory/2: Fill the memory with 0s
fill_memory([], 100, []).
fill_memory([], OldNumber, [0 | M]) :-
    NewNumber is OldNumber + 1,
    fill_memory([], NewNumber, M), !.
fill_memory([M | Mem], N, [M | FilledMem]) :-
    fill_memory(Mem, N, FilledMem).



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
    % Compiled succesfully
    writeln('Msg: Compiled succesfully.'),
    % Fill the memory with 0s
    length(MemResolved, X),
    fill_memory(MemResolved, X, Mem).



%%% lmc_run/3
lmc_run(Filename, Input, Output) :-
    lmc_load(Filename, Mem),

%%% one_instruction/2: Given a state return the new State
one_instruction(State, NewState) :-
    fail.


%%% execution_loop/2
execution_loop(State, Out) :-
    fail


/*

mc_load(Filename, Mem)
dove Filename è il nome di un file e Mem è la memoria del sistema nel suo "stato iniziale”.
Il secondo è un predicato dal nome lmc_run/3 che si preoccupa di leggere un file che
contiene un codice assembler, lo carica (con lmc_load/2), imposta la coda di input al valore
fornito e produce un output che è il risultato dell’invocazione di execution_loop/2.
?- lmc_run(”my/prolog/code/lmc/test-assembly-42.lmc”, [42], Output)
è un esempio della sua invocazione.
Suggerimenti
Per implementare al meglio (ed in modo semplice) il simulatore LMC in Prolog vi consigliamo
come minimo, di utilizzare il predicato nth0/4.
Un modo per generare uno “stato” iniziale è di usare il predicato di libreria randseq/3;
attenzione che il primo argomento non può essere superiore al secondo. Ad esempio:
?- randseq(99, 99, Mem).
Mem = [33, 10, 64, 29, 62, 53, 98, 22, 36|...].

*/
