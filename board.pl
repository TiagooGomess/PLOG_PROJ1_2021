:- use_module(library(random)).
:- use_module(library(lists)).

:-ensure_loaded('utils.pl').

createBoard([[]], 0, _).
createBoard(Board, Size, Pieces):-
	Size > 0,
	Size1 is Size - 1,
	createLine(6, Pieces, Line),
	remove_elements(Pieces, Line, RemainingPieces),
	Board = [Line | T],
	createBoard(T, Size1, RemainingPieces).
	
% copia N elementos de L1 (lista com todas as peças restantes) para L2 ([X|T]) de forma aleatória
createLine(0, _, []).
createLine(N, L1, [X|T]):-
	N > 0,
	N1 is N - 1,
	random_member(X, L1),
	select(X, L1, Remaining),
	createLine(N1, Remaining, T).
	
printBoard([]).
printBoard([H|T]):-
	printLine(H),
	nl,
	printBoard(T).
	
printLine([]).
printLine([H|T]):-
	write('|'),
	write(H),
	write('|'),
	printLine(T).