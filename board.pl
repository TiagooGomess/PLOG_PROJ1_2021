:- use_module(library(random)).
:- use_module(library(lists)).

:-ensure_loaded('utils.pl').

% cria uma tabuleiro, no formato lista de listas de listas, em que as listas mais interiores
% são formadas por apenas um elemento, já que no início as stacks têm todas uma altura de 1
createBoard([[]], 0, _).
createBoard(Board, Size, Pieces):-
	Size > 0,
	Size1 is Size - 1,
	createLine(6, Pieces, Line),
	remove_elements(Pieces, Line, RemainingPieces),
	Board = [Line | T],
	createBoard(T, Size1, RemainingPieces).
	
% copia N elementos de L1 (lista com todas as peças restantes) para L2 ([[X]|T]) de forma aleatória
% [[X]|T] é uma lista de listas; esta função cria uma linha em que cada célula da linha possui
% uma lista com um elemento
createLine(0, _, []).
createLine(N, L1, [[X]|T]):-
	N > 0,
	N1 is N - 1,
	random_member(X, L1),
	select(X, L1, Remaining),
	createLine(N1, Remaining, T).
	
% imprime o tabuleiro
printBoard([]).
printBoard([H|T]):-
	printLine(H),
	nl,
	printBoard(T).
	
% imprime a Head de cada lista da linha
printLine([]).
printLine([[H|_]|T]):-
	write('|'),
	write(H),
	write('|'),
	printLine(T).