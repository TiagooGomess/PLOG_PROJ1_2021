:- use_module(library(random)).

createBoard([[]], 0).
createBoard(Board, Size):-
	Size > 0,
	Size1 is Size - 1,
	createLine(Line, 6),
	Board = [Line | T],
	createBoard(T, Size1).
	
createLine([], 0).
createLine(Line, Size):-
	Size > 0,
	Size1 is Size - 1,
	random(0,3,PieceNum), % PieceNum is a random number (0, 1 or 2)
	Line = [PieceNum | T],
	createLine(T, Size1).
	
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
	