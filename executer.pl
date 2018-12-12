%%% execute_instruction/4: Execute a single instruction
% add
execute_instruction(1, Arg, State, NewState) :-
    State =.. [StateType, Acc, PC, Mem, Input, Out, _],
    % Get target memory cell
    nth0(Arg, Mem, Cell),
    % Execute add
    EvaluateAcc is Acc + Cell,
    % Set the flag
    evaluate_flag(EvaluateAcc, NewFlag),
    NewAcc is mod(EvaluateAcc, 1000),
    % Increment the PC
    increment_pc(PC, NewPC),
    % Create the new state
    NewState =.. [StateType, NewAcc, NewPC, Mem, Input, Out, NewFlag].
% sub
execute_instruction(2, Arg, State, NewState) :-
    State =.. [StateType, Acc, PC, Mem, Input, Out, _],
    % Get target memory cell
    nth0(Arg, Mem, Cell),
    % Execute sub
    EvaluateAcc is Acc - Cell,
    % Set the flag
    evaluate_flag(EvaluateAcc, NewFlag),
    NewAcc is mod(EvaluateAcc, 1000),
    % Increment the PC
    increment_pc(PC, NewPC),
    % Create the new state
    NewState =.. [StateType, NewAcc, NewPC, Mem, Input, Out, NewFlag].
% sta
execute_instruction(3, Arg, State, NewState) :-
    State =.. [StateType, Acc, PC, Mem, Input, Out, Flag],
    % Store the Acc value in memory
    replace_pos(Mem, Arg, Acc, NewMem),
    % Increment the PC
    increment_pc(PC, NewPC),
    % Create the new state
    NewState =.. [StateType, Acc, NewPC, NewMem, Input, Out, Flag].
% lda
execute_instruction(5, Arg, State, NewState) :-
    State =.. [StateType, _, PC, Mem, Input, Out, Flag],
    nth0(Arg, Mem, NewAcc),
    % Increment the PC
    increment_pc(PC, NewPC),
    % Create the new state
    NewState =.. [StateType, NewAcc, NewPC, Mem, Input, Out, Flag].
% bra
execute_instruction(6, Arg, State, NewState) :-
    State =.. [StateType, Acc, _, Mem, Input, Out, Flag],
    % Jump
    NewPC = Arg,
    % Create the new state
    NewState =.. [StateType, Acc, NewPC, Mem, Input, Out, Flag].
% brz
execute_instruction(7, Arg, State, NewState) :-
    State =.. [StateType, Acc, _, Mem, Input, Out, Flag],
    % Brz condition
    Acc = 0, Flag = noflag, !,
    % Jump
    NewPC = Arg,
    % Create the new state
    NewState =.. [StateType, Acc, NewPC, Mem, Input, Out, Flag].
execute_instruction(7, Arg, State, NewState) :-
    State =.. [StateType, Acc, _, Mem, Input, Out, Flag],
    % Don't jump, just increment PC by 1
    NewPC = PC + 1,
    % Create the new state
    NewState =.. [StateType, Acc, NewPC, Mem, Input, Out, Flag].
% brp
execute_instruction(8, Arg, State, NewState) :-
    State =.. [StateType, Acc, _, Mem, Input, Out, Flag],
    % Brp condition
    Flag = noflag, !,
    % Jump
    NewPC = Arg,
    % Create the new state
    NewState =.. [StateType, Acc, NewPC, Mem, Input, Out, Flag].
execute_instruction(8, Arg, State, NewState) :-
    State =.. [StateType, Acc, _, Mem, Input, Out, Flag],
    % Don't jump, just increment PC by 1
    NewPC = PC + 1,
    % Create the new state
    NewState =.. [StateType, Acc, NewPC, Mem, Input, Out, Flag].
/*
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


%%% evaluate_flag/2: set flag if overflow or underflow 
evaluate_flag(Acc, NewFlag) :-
    Acc >= 1000,
    NewFlag = flag.
evaluate_flag(Acc, NewFlag) :-
    Acc < 0,
    NewFlag = flag.
evaluate_flag(Acc, NewFlag) :-
    Acc >= 1000,
    NewFlag = noflag.



%%% increment_pc/2: Increment programm counter
increment_pc(PC, NewPC) :-
    PCinc is PC + 1,
    NewPC is mod(PCinc, 1000).



%%% replace_pos/4: Change an element in a list
replace_pos(List, Pos, Symbol, NewList) :-
    nth0(Pos, List, _, Rest),
    nth0(Pos, NewList, Symbol, Rest).


/*
1xx Addizione Somma il contenuto della cella di memoria xx con il valore contenuto
nell’accumulatore e scrive il valore risultante nell’accumulatore. Il
valore salvato nell’accumulatore è la somma modulo 1000. Se la
somma non supera 1000 il flag è impostato ad assente, se invece
raggiunge o supera 1000 il flag è impostato a presente.
2xx Sottrazione Sottrae il contenuto della cella di memoria xx dal valore contenuto
nell’accumulatore e scrive il valore risultante nell’accumulatore. Il
valore salvato nell’accumulatore è la differenza modulo 1000. Se la
differenza è inferiore a zero il flag è impostato a presente, se invece è
positiva o zero il flag è impostato ad assente.
3xx Store Salva il valore contenuto nell’accumulatore nella cella di memoria
avente indirizzo xx. Il contenuto dell’accumulatore rimane invariato.
5xx Load Scrive il valore contenuto nella cella di memoria di indirizzo xx
nell’accumulatore. Il contenuto della cella di memoria rimane invariato.
6xx Branch Salto non condizionale. Imposta il valore del program counter a xx.
7xx Branch if zero Salto condizionale. Imposta il valore del program counter a xx
solamente se il contenuto dell’accumulatore è zero e se il flag è
assente.
8xx Branch if positive Salto condizionale. Imposta il valore del program counter a xx
solamente se il flag è assente.
901 Input Scrive il contenuto presente nella testa della coda in input
nell’accumulatore e lo rimuove dalla coda di input.
902 Output Scrive il contenuto dell’accumulatore alla fine della coda di output. Il
contenuto dell’accumulatore rimane invariato.
0xx Halt Termina l’esecuzione del programma. Nessuna ulteriore istruzione
viene eseguita

*/











/*

lmc_load(Filename, Mem)
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