%%%% -*- Mode: Prolog -*-

/*
little_man_computer.pl
Written by: Gianluca Giudice.
*/



/*
instruction/3: List of instructions (word reserved).
First   argument = reserved word for instruction.
Second  argument = machine code relative to the instruction.
Third   argument = argument of the instruction
Fourth  argument = Current
*/

/*
Tutte queste istruzioni hannno una implicazione ovvero la loro esecuzione
*/
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

valid_argument(Arg) :- Arg =< 99, Arg >= 0.
compile_instruction(add, Arg, Code) :- valid_argument(Arg), Code is 100 + Arg.
compile_instruction(sub, Arg, Code) :- valid_argument(Arg), Code is 200 + Arg.
compile_instruction(sta, Arg, Code) :- valid_argument(Arg), Code is 300 + Arg.
compile_instruction(lda, Arg, Code) :- valid_argument(Arg), Code is 500 + Arg.
compile_instruction(bra, Arg, Code) :- valid_argument(Arg), Code is 600 + Arg.
compile_instruction(brz, Arg, Code) :- valid_argument(Arg), Code is 700 + Arg.
compile_instruction(brp, Arg, Code) :- valid_argument(Arg), Code is 800 + Arg.
compile_instruction(dat, Code, Code) :- Code =< 999, Code >= 0.
compile_instruction(dat, Code) :- compile_instruction(dat, 0, Code).
compile_instruction(hlt, 0).
compile_instruction(inp, 901).
compile_instruction(out, 902).


/*
findall(X, compile_instruction(X, _), Y).
*/
%%% word_reserverd/1: True if Word is reserved to compiler
word_reserverd(Word) :-
    atom_string(AtomWord, Word), compile_instruction(AtomWord, 0, _).
word_reserverd(Word) :-
    atom_string(AtomWord, Word), compile_instruction(AtomWord, _).




%%% defined_label/2: Label defined in the program. (labelName, MemPointer)
defined_label('', '').      % Placeholder
%%% undefined_label/2: Label undefined in the program. (labelName, MemPointer)
undefined_label('', '').    % Placeholder


/*
        MANIPOLAZIONE BASE DI CONOSCENZA = Assert e retract
undefined_label = label ancora non usate
appena la trovo la aggiungo, se non la utilizzo la tolgo

defined_label = label utilizzate con relativa posizione in memoria
*/


%%% assembler/4: Convert whole assembly program into machine code.
% Check if all label defined
% Compiled succesfully
assembler([], _, []).
% Memory overflow
assembler(_, MemPointer, _) :-
    MemPointer >= 100,
    writeln('COMPILE ERROR: Too much instructions to load in memory.'), !, fail.
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


%%% split_assembly_line/2 split assembly line into a list of single word
split_assembly_line(Line, SplittedLine) :-
    % Remove comment in line.
    /*
    Change remove comment
    */
    % The char "/" is allowed only in comment so is possible to use split_string
    split_string(Line, "/", "/", [InstructionNoComment | _]),
    % Split string using whitespaces
    split_string(InstructionNoComment, " \t", " \t", SplittedLine).



%%% assembler_line/3: Convert a single instruction into machine code.
/*
Evaluate the length of the list containing the single assembly line
    - Len = 1:
        a) [instruction].
            An instruction without argument
    - Len = 2:
        a) [instruction - integerArgument]
            Instruction with an integer argument
        b) [instruction - labelArgument]
            Instruction with an label   argument
        c) [label - instructionWhitoutArgument]
            A label followed by an instruction without arguments
    - Len = 3:
        a) [label - instruction - argument]
            Label followed by an instruction and its argument
*/
% [1.a] = Instruction without argument
assembler_line([Instruction], _, MachineCode) :-
    atom_string(AtomInstruction, Instruction),
    compile_instruction(AtomInstruction, MachineCode), !.
% [2.a] = Instruction followed by an integer as argument
assembler_line([Instruction, Argument], _, MachineCode) :-
    % May be a valid instruction
    atom_string(AtomInstruction, Instruction),
    number_string(ArgumentValue, Argument),
    compile_instruction(AtomInstruction, ArgumentValue, MachineCode), !.
% [2.b] = Instruction folllowed by a label as argument
assembler_line([Instruction, Label], MemPointer, MachineCode) :-
    word_reserverd(Instruction), \+ word_reserverd(Label),
    % May be a valid instruction
    assembler_line([Instruction, "0"], _, MachineCode),
    evaluate_label(Label, MemPointer, undefined_label), !.
% [2.c] = Label followed by an instruction without argument
assembler_line([Label, Instruction], MemPointer, MachineCode) :-
    word_reserverd(Instruction), \+ word_reserverd(Label),
    % May be a valid instruction
    assembler_line([Instruction], _, MachineCode),
    evaluate_label(Label, MemPointer, defined_label), !.
% [3.a] = Label followed by an instruction and its argument
assembler_line([Label, Instruction, Argument], MemPointer, MachineCode) :-
    word_reserverd(Instruction), \+ word_reserverd(Label),
    % May be a valid instruction
    assembler_line([Instruction, Argument], MemPointer, MachineCode), !,
    evaluate_label(Label, MemPointer, defined_label).
% If not unify with other, then is an invalid instruction
assembler_line(_, _, _) :-
    writeln('COMPILE ERROR: Invalid instruction'), fail.



%%% evaluate_label/3: Add a label to knowledge base
% Label alredy defined

% alnum. controlla se alfanumerico
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
    % If the only undefined_label is placeholder, the all labels are resolved
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



%%% lmc_load/2: Given a file, return the content of the memory.
lmc_load(Filename, Mem) :-
    open(Filename, read, Input),
    read_string(Input, _, OutputString),
    % Convert ouput file string to lowercase
    string_lower(OutputString, OutputStringLower),
    % Split output file string into a list of rows (Windows and linux support)
    split_string(OutputStringLower, '\n', '\r', LineList),
    % Convert assembly programm into machine code starting from line 0
    assembler(LineList, 0, MemUnresolved),
    % Resolve all undefined label
    findall(X, defined_label(X, _), [_ | DefinedLabelList]),
    resolve_labels(DefinedLabelList, MemUnresolved, Mem),
    % Compiled succesfully
    writeln('Msg: Compiled succesfully.'),

/*
% add the last memory cells
% add the last memory cells
% add the last memory cells

*/


    close(Input).
