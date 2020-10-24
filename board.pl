:- use_module(library(random)).

createBoard([[]], 0, _).
createBoard(Board, Size, Pieces):-
	Size > 0,
	Size1 is Size - 1,
	createLine(Line, 6, Pieces),
	Board = [Line | T],
	createBoard(T, Size1, Pieces).
	
createLine([], 0, _).
createLine(Line, Size, Pieces):-
	Size > 0,
	Size1 is Size - 1,
	random_select(PieceNum,Pieces, R),
	%printLine(R), 
	%nl,
	Line = [PieceNum | T],
	createLine(T, Size1, R).
	
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
