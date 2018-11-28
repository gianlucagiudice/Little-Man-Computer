%%%% -*- Mode: Prolog -*-

%%%% parse-stuff.pl
%%%%
%%%% Remember that you are always really "following" a state machine.


%%% is_digit/1, is_white/1

is_digit(C) :- char_type(C, digit).

is_white(C) :- char_type(C, white).


%%% skip_whitespaces/2
%%% skip_whitespaces(Input, MoreInput)

skip_whitespaces([C | Cs], MoreInput) :-
    is_white(C),
    !,

    skip_whitespaces(Cs, MoreInput).
skip_whitespaces([C | Cs], [C | Cs]) :-
    \+ is_white(C),
    !.

skip_whitespaces([], []) :- !.


%%% parse_int/3

parse_int(Input, I, MoreInput) :-
    skip_whitespaces(Input, PossibleIntInput),
    parse_int(PossibleIntInput,
	      [],
	      I,
	      _,
	      MoreInput).

parse_int([], DigitsSoFar, I, DigitCodes, []) :-
    !,
    reverse(DigitsSoFar, DigitCodes),
    number_string(I, DigitCodes).

parse_int([C | Cs], DigitsSoFar, I, DigitCodes, Rest) :-
    is_digit(C),
    !,
    parse_int(Cs, [C | DigitsSoFar], I, DigitCodes, Rest).

parse_int([C | Cs], DigitsSoFar, I, DigitCodes, [C | Cs]) :-
    \+ is_digit(C),
    !,
    reverse(DigitsSoFar, DigitCodes),
    number_string(I, DigitCodes).


%%% parse_float/3, parse_float/4

parse_float(Input, F, MoreInput) :-
    skip_whitespaces(Input, PossibleFloatInput),
    parse_float(PossibleFloatInput,
		F,
		_FloatCodes,
		MoreInput).

parse_float([0'+ | FloatChars],
	    F,
	    FloatCodes,
	    MoreInput) :-
    !,
    parse_float(FloatChars, F, FloatCodes, MoreInput).

parse_float([0'- | FloatChars],
	    NegF,
	    FloatCodes,
	    MoreInput) :-
    !,
    parse_float(FloatChars, F, FloatCodes, MoreInput),
    NegF is -F.
parse_float(Input, F, FloatCodes, MoreInput) :-
    parse_int(Input, [], _IntPart, IntCodes, RestInput),
    parse_decimal_part(RestInput,
		       _Decimal,
		       DecimalCodes,
		       MoreInput),
    append(IntCodes, DecimalCodes, FloatCodes),
    number_string(F, FloatCodes).



%%% parse_decimal_part/4

parse_decimal_part([C | DecimalInput],
		   1.0,
		   [],
		   [C | DecimalInput]) :-
    C \= 0'., !.

parse_decimal_part([0'. | DecimalInput],
		   Decimal,
		   [0'. | DecimalChars],
		   MoreInput) :-
    !,
    parse_int(DecimalInput,
	      [],
	      I,
	      DecimalChars,
	      MoreInput),
    length(DecimalChars, DCL),
    Decimal is I / (10.0 ** DCL).


%%%% end of file -- parse-stuff.pl
