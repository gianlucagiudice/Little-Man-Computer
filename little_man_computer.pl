%%%% -*- Mode: Prolog -*-

/*
little_man_computer.pl
Written by: Gianluca Giudice.
*/


/*
%%% is_keyword_reserved/1: return if keyword is reserved to compiler.
is_keyword_reserved(X) :-
    % --- ATTENZIONE!!! PROBABILMENTE IL CUT È INUTILE ---
    member(X, ["ADD", "SUB", "STA", "LDA", "BRA", "BRZ", "BRP", "INP", "OUT", "HLT", "DAT", "DAT"]), !.

:- consult("nmome")


guitracer

*/

/*
instruction/3: List of instructions (word reserved).
First argument  = reserved word for instruction.
Second argument = machine code relative to the instruction.
Third argument  = argument of the instruction
*/

/*
Forse potrei utilizzare "_" anzichè X. Questo perchè a me serve
sapere se una particolare istruzione ammette o no argomenti,
non mi interessa l'argomento nello specifico.

Tutte queste istruzioni hannno una implicazione ovvero la loro esecuzione
*/
instruction(dat, 0, Arg, State).
instruction(add, 1, Arg, State).
instruction(sub, 2, Arg, State).
instruction(sta, 3, Arg, State).
instruction(lda, 5, Arg, State).
instruction(bra, 6, Arg, State).
instruction(brz, 7, Arg, State).
instruction(brp, 8, Arg, State).
instruction(dat, 0, Arg, State).
instruction(dat, 0, Arg, State).
instruction(hlt, 0, Arg, State).
instruction(inp, 901, Arg, State).
instruction(out, 902, Arg, State).
% explode state ritorna una lista contenente gli argomenti di state()



/*
        MANIPOLAZIONE BASE DI CONOSCENZA = Assert e retract
undefined_label = label ancora non usate
appena la trovo la aggiungo, se non la utilizzo la tolgo

defined_label = label utilizzate con relativa posizione in memoria
*/

/*
Errori:
    - Unexpected character
    - Illegal instruction "Stampa istruzione e riga"
Prima faccio il parsing avendo una lista con sole istruzione.
poi guardo se l'eleemtno in testa alla lista è riservato, altrimenti è una label.
Se dopo il primo elemento non ho una reserved keyword allora ho errore.
Per ogni comando devo vedere se quel comando necessita argomenti, se non ne necessita allora ho errore

Devo aggiungere alla base di conoscenza tutti le istruzioni valide Es:
legal_instruction(9, X, Y)
legal_instruction(6, X, Y)
legal_instruction(3, X, Y)
Se non unifica allora l'istruzione non è valida

Devo guardare la lunghezza delle liste relative alle righe:
    - Len = 1:
        Istruzione senza argomenti e provo ad unificare
    - Len = 2:
        a) Istruzione con argomento tipo Numero
        c) Istruzione tipo label con argomento
        b) Istruzione con argomento tipo label
            aggiungo label alle undefined_label, con riferimento alla riga
    - Len = 3:
        Lista con label iniziale del tipo:
            Label - istruzione -argomenti
            Se non unifica allora fallisce, vuol dire che non è valida
*/



%%% evaluate_line/3: Given a list of string codes, get the first line

is_white(C) :- char_type(C, white).


%%% skip_whitespaces/2
%%% skip_whitespaces(Input, MoreInput)

/*
skip_whitespaces([C | Cs], MoreInput) :-
    is_white(C), !, skip_whitespaces(Cs, MoreInput).
skip_whitespaces([C | Cs], [C | Cs]) :-
    \+ is_white(C),
    !.

skip_whitespaces([], []) :- !.

%%% parse_command/2: Parse the command, return a list
parse_command(Command, CommandList) :-
    fail.
*/

%%% assembler/2: Convert assembly program into machine code.
% Skip blank line
assembler([Row | RestFile], LinePointer, Mem) :-
    % Find all whitespace
    findall(X, char_type(X, white), White), member(X, White),
    split_string(Row, X, X, [Splitted | _]),
    % If Line is blank skip it
    Splitted = "", !,
	assembler(RestFile, LinePointer, Mem).
assembler([Row | RestFile], LinePointer, [MachineCodeLine | Mem]) :-
    MachineCodeLine is LinePointer,
    NewLinePointer is LinePointer + 1,
    assembler(RestFile, NewLinePointer, Mem).
    /*
    % Split the ro into a list of strings
    split_string("a \tb  \t\t  \tu \t\t\t\t\t\t   c  \t   d", "\t ", " \t", L).

    % Get the codes of a single line
    evaluate_line(File_codes, Line_codes, Rest_file_codes),
    % Parse the string list and return a list of atoms
    pasre_line(Line_codes, Line_list),
    % Convert single line list into machine code
    line_to_machineCode(Line_list, MachineCode_line),
    New_row is Row + 1,
    assembler_to_machineCode(Rest_file_codes, New_row, Mem).
    */


/*
% if no label defined fail
assembler([], LinePointer, Mem) :-
    % if no label defined fail
    !,
    fail.

% warning for label never used
assembler([], LinePointer, Mem) :-
    % Print warning for label never used
    % NO CUT!!!!
    fail.
*/

% Compile succesfully
assembler([], LinePointer, []) :-
    write("Compiled succesfully!").


%%% lmc_load/2: Given a file, return the content of the memory.
lmc_load(Filename, Mem) :-
    open(Filename, read, Input),
    read_string(Input, _, OutputString),
    % Convert ouput file string to lowercase
    string_lower(OutputString, OutputStringLower),
    % Split output file string into a list of rows (Windows and linux support)
    split_string(OutputStringLower, '\n', '\r', RowList),
    write(RowList),
    % Convert assembly programm into machine code starting from row 0
    assembler(RowList, 0, MemUndefined),
    write(MemUndefined),
    % resolve_label(MemUndefined, Mem),
    close(Input).

    /*

    string_codes(OutputStringLower, OutputString_codes),
    write(OutputStringLower),
    write(OutputString_codes),
    */
