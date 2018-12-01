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
First   argument = reserved word for instruction.
Second  argument = machine code relative to the instruction.
Third   argument = argument of the instruction
Fourth  argument = Current
*/

/*
Tutte queste istruzioni hannno una implicazione ovvero la loro esecuzione
*/
instruction(dat, 0, Arg, State, NewState).
instruction(add, 1, Arg, State, NewState).
instruction(sub, 2, Arg, State, NewState).
instruction(sta, 3, Arg, State, NewState).
instruction(lda, 5, Arg, State, NewState).
instruction(bra, 6, Arg, State, NewState).
instruction(brz, 7, Arg, State, NewState).
instruction(brp, 8, Arg, State, NewState).
instruction(dat, 0  , State, NewState).
instruction(dat, 0  , State, NewState).
instruction(hlt, 0  , State, NewState).
instruction(inp, 901, State, NewState).
instruction(out, 902, State, NewState).
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


%%% split_assembly_line/2 spit line into a list of single word
split_assembly_line(Line, SplittedLine) :-
    % Find all whitespace
    findall(X, char_type(X, white), White), member(X, White),
    % Split string using whitespaces
    split_string(Line, X, X, SplittedLine).

/*

%%% assembler/2: Convert assembly program into machine code.
% Skip blank line
assembler([Line | RestFile], LinePointer, Mem) :-
    split_assembly_line(Line, [HeadSplittedLine | _]),
    % If Line is blank skip it
    HeadSplittedLine = "", !,
	assembler(RestFile, LinePointer, Mem).
% Convert each line into machine code
assembler([Line | RestFile], LinePointer, [MemLine | Mem]) :-
    split_assembly_line(Line, SplittedLine),
    assembler_line(SplittedLine, MemLine), !,
    NewLinePointer is LinePointer + 1,
    assembler(RestFile, NewLinePointer, Mem).

*/
% If not unify with other, then is a compile error
assembler([Line | RestFile], LinePointer, [MemLine | Mem]) :-
    format('COMPILE ERROR: Invalid instruction\nat line ~d: "~s".\n', [LinePointer, Line]),
    fail.
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
    write("Msg: Compiled succesfully."), nl.


%%% lmc_load/2: Given a file, return the content of the memory.
lmc_load(Filename, Mem) :-
    open(Filename, read, Input),
    read_string(Input, _, OutputString),
    % Convert ouput file string to lowercase
    string_lower(OutputString, OutputStringLower),
    % Split output file string into a list of rows (Windows and linux support)
    split_string(OutputStringLower, '\n', '\r', LineList),
    write(LineList), nl,
    % Convert assembly programm into machine code starting from line 0
    assembler(LineList, 0, MemUndefined),
    write(MemUndefined),
    % resolve_label(MemUndefined, Mem),
    close(Input).

    /*

    string_codes(OutputStringLower, OutputString_codes),
    write(OutputStringLower),
    write(OutputString_codes),
    */
