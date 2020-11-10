:- use_module(library(random)).
:- use_module(library(lists)).

:-ensure_loaded('utils.pl').

% cria um tabuleiro, no formato lista de listas de listas, em que as listas mais interiores
% são formadas por apenas um elemento, já que no início as stacks têm todas uma altura de 1
createBoard([[]], 0, _).
createBoard(Board, Size, Pieces):-
	Size > 0,
	Size1 is Size - 1,
	createLine(6, Pieces, Line),
	remove_elements(Pieces, Line, RemainingPieces),
	Board = [Line | T],
	createBoard(T, Size1, RemainingPieces).
	
% copia N elementos de L1 (lista com todas as peças restantes) para [[X]|T] de forma aleatória
% [[X]|T] é uma lista de listas; esta função cria uma linha em que cada célula da linha possui
% uma lista com um elemento
createLine(0, _, []).
createLine(N, L1, [[X]|T]):-
	N > 0,
	N1 is N - 1,
	random_member(X, L1),
	select(X, L1, Remaining),
	createLine(N1, Remaining, T).
	
% imprime o tabuleiro, incluindo os números das linhas e colunas
printBoard(_,[]):-
	write('  |-----|-----|-----|-----|-----|-----|'),nl,
    write('     A     B     C     D     E     F ').
printBoard([H|T],[Row|RowT]):-
	write('  |-----|-----|-----|-----|-----|-----|'),nl,
	write(Row),
	write(' '),
	printLine(H),
	write('|'),
	nl,
	printBoard(T,RowT).
	
% imprime a Head de cada lista da linha e o número de pirâmides verdes, em cada célula (caso existam)
printLine([]).
printLine([[H|_T]|T]):-
	write('|'),
	occurrences_of([H|_T],2,NumGreen),
	translate(H, Piece),
	writeInCell(Piece, NumGreen),
	printLine(T).

% Escreve o número de pirâmides verdes de uma stack, caso existam
writeNumGreen(0):-
	write(' ').
writeNumGreen(Num):-
	write(Num).

% escreve a informação na célula do tabuleiro
writeInCell(Piece, 0):-
	write(' '),
	write(Piece),
	write(' ').
writeInCell(Piece, NumGreen):-
	write(Piece),
	write(NumGreen),
	write(' ').

% dá-nos o número da peça que está no topo da stack, dada uma linha e uma coluna
getPieceByRowAndColumn(Board, Row, Column, Piece):-
    nth0(Row,Board,RowList),
    nth0(Column,RowList,[Piece|_]).

% pergunta ao jogador qual a posição da peça que quer mover
askForPiecePos(Row, Column, Question):-
	repeat,
	nl,nl,
	write(Question),nl,nl,
	write('Column: '),
	getChar(Input1),
	translate_column(Input1, Column),
	write('Row: '),
	getInt(Input2),
	Input2 =< 6,
	Input2 >= 1,
	translate_row(Input2, Row).

% pergunta ao jogador a peça que quer mover e para que posição
askMove(Board, RowStart, ColumnStart, RowEnd, ColumnEnd):-
	askForPiecePos(RowStart, ColumnStart, 'Which stack do you want to move?'),nl,nl,
	askForPiecePos(RowEnd, ColumnEnd, 'For which position do you want to move it?'),nl,nl,
	getPieceByRowAndColumn(Board, RowStart, ColumnStart, Piece),
	translate(Piece, PieceChar),
	translate_row(RowStartVisible, RowStart),
	translate_column(ColumnStartVisible, ColumnStart),
	translate_row(RowEndVisible, RowEnd),
	translate_column(ColumnEndVisible, ColumnEnd),nl,nl,
	write('You are moving piece'),
	write(PieceChar),
	write('from ('),
	write(ColumnStartVisible),
	write(','),
	write(RowStartVisible),
	write(')'),
	write(' to ('),
	write(ColumnEndVisible),
	write(','),
	write(RowEndVisible),
	write(')').
