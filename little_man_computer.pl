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

%%% Include files
:- consult(compiler).



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
    lmc_load(Filename, Mem).

%%% one_instruction/2: Given a state return the new State
one_instruction(State, NewState) :-
    fail.


%%% execution_loop/2
execution_loop(State, Out) :-
    fail.


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
