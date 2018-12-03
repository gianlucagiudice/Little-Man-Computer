%%%% -*- Mode: Prolog -*-

/*
little_man_computer.pl
Written by: Gianluca Giudice.
*/


/*

guitracer

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

%%% word_reserverd/1: True if Word is reserved to compiler
word_reserverd(Word) :-
    (compile_instruction(Word, 0, _) ; compile_instruction(Word, _)).

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
assembler([], _, []) :-
    writeln('Msg: Compiled succesfully.').
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
% [1.a] = Not valid
assembler_line([_], _, _) :- fail.
% [2.a] = Instruction followed by an integer as argument
assembler_line([Instruction, Argument], _, MachineCode) :-
    atom_string(AtomInstruction, Instruction),
    number_string(ArgumentValue, Argument), !,
    compile_instruction(AtomInstruction, ArgumentValue, MachineCode).
% [2.a] = Not valid
assembler_line([_, _], _, _) :- fail.
% [2.b] = Instruction folllowed by a label as argument
assembler_line([Instruction, Label], MemPointer, MachineCode) :-
    assembler_line([Instruction, "0"], _, MachineCode),
    atom_string(AtomLabel, Label), !,
    evaluate_label(AtomLabel, MemPointer, undefined_label).
% [2.c] = Label followed by an instruction without argument
assembler_line([Label, Instruction], MemPointer, MachineCode) :-
    assembler_line([Instruction], _, MachineCode),
    atom_string(AtomLabel, Label), !,
    evaluate_label(AtomLabel, MemPointer, defined_label).
% [3.a] = Label followed by an instruction and its argument
assembler_line([Label, Instruction, Argument], MemPointer, MachineCode) :-
    atom_string(AtomInstruction, Instruction),
    word_reserverd(AtomInstruction),
    % May be a valid instruction
    assembler_line([Instruction, Argument], MemPointer, MachineCode),
    atom_string(AtomLabel, Label), !,
    evaluate_label(AtomLabel, MemPointer, defined_label).
% If not unify with other, then is an invalid instruction
assembler_line(_, MemPointer, _) :-
    writeln('COMPILE ERROR: Invalid instruction'), fail.



%%% evaluate_label/3: Add a label to knowledge base
% Label alredy defined
evaluate_label(Label, _, defined_label) :-
    % Label can not be alredy defined
    defined_label(Label, _), !,
    format('COMPILE ERROR: Label "~s" is alredy defined.\n', [Label]), fail.
evaluate_label(Label, _, _) :-
    % Label can not have the same name as instruction.
    word_reserverd(Label), !,
    % Label can not be alredy defined
    format('COMPILE ERROR: Label "~s" is a reserved word.\n', [Label]), fail.
% Add label to knowledge base
evaluate_label(Label, MemPointer, LabelType) :-
    % Create assertz = assertz(labelType(AtomLabel, MemPointer))
    % Create the functor to add in knowledge base
    functor(Func, LabelType, 2),
    arg(1, Func, Label),
    arg(2, Func, MemPointer),
    % Assert the functor
    functor(Assert, assertz, 1),
    arg(1, Assert, Func),
    call(Assert).



%%% resolve_labels/2: Convert a label to corresponding value
resolve_labels([], Mem, Mem).
resolve_labels(DefinedLabelList, MemUnresolved, Mem).
/*
resolve_labels(MemUnresolved, Mem) :-
    bagof(Row, undefined_label(LabelName, Row), Rows),
    defined_label(LabelName, LabelValue),
    write(Rows),
    resolve_one_label(Rows, MemUnresolved, Mem),
    % Check if all labels are been resolved
    findall(X, undefined_label(X, _), Y), length(Y, 1), !.
% If some label are still undefined
resolve_labels(_, _) :-
    writeln('COMPILE ERROR: Label undefined'),
    setof(LabelName, Row^defined_label(LabelName, Row), [_ | UndefinedLabel]),
    write("List of label: "), writeln(UndefinedLabel), !, fail.
*/
%%% resolve_one_label/2: Resolve all values relative to a single label
resolve_one_label([], Mem, Mem).
resolve_one_label([Row | List], MemUnresolved, Mem) :-
    nth0(Row, MemUnresolved, OldValue, Rest),
    NewValue is OldValue + Row,
    nth0(Row, Mem, NewValue, Rest),
    resolve_one_label(List, Mem, Mem).



%%% lmc_load/2: Given a file, return the content of the memory.
lmc_load(Filename, Mem) :-
    open(Filename, read, Input),
    read_string(Input, _, OutputString),
    % Convert ouput file string to lowercase
    string_lower(OutputString, OutputStringLower),
    % Split output file string into a list of rows (Windows and linux support)
    split_string(OutputStringLower, '\n', '\r', LineList),
    writeln(LineList),
    % Convert assembly programm into machine code starting from line 0
    assembler(LineList, 0, MemUnresolved),
    writeln(MemUnresolved),
    % Resolve all undefined label

    % FINDALL UNRESOLVED LABEL and start to resolve
    % resolve_labels(DefinedLabel, MemUnresolved, Mem),

    writeln(Mem),
    close(Input).
