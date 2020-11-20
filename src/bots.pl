:-ensure_loaded('utils.pl').
:-ensure_loaded('board.pl').
:-ensure_loaded('game.pl').

:- use_module(library(between)).

% este predicado dá-nos um movimento aleatório válido
getMoveEasy(GameState, Player, RowStart, ColumnStart, RowEnd, ColumnEnd):-
    getPiecePosFrom(GameState, Player, RowStart, ColumnStart),
	getPiecePosTo(GameState, RowStart, ColumnStart, RowEnd, ColumnEnd).

% este predicado dá-nos uma linha e uma coluna aleatórias, entre 0 e 5
getRandomRowAndColumn(Row,Column):-
    between(0,5,Row),
    between(0,5,Column).

% escolhe uma peça válida para mover de forma aleatória
getPiecePosFrom(Board, Player, Row, Column):-
	repeat,
	getRandomRowAndColumn(Row,Column),
	(
		(
			getPieceByRowAndColumn(Board, Row, Column, Piece),
   	 		Piece = Player, % verifica se a stack pertence ao jogador atual
			\+ checkIfStackCannotCapture(Board, Row, Column),! % verifica se a stack pode capturar outras stacks
		);
		fail
	).

% escolhe uma posição final válida de forma aleatória
getPiecePosTo(Board, RowFrom, ColumnFrom, RowTo, ColumnTo):-
	repeat,
	getRandomRowAndColumn(RowTo,ColumnTo),
	(
		(
			checkOrthogonality(RowFrom, ColumnFrom, RowTo, ColumnTo), % verifica se o movimento é feito ortogonalmente
			checkStacksBetween(Board, RowFrom, ColumnFrom, RowTo, ColumnTo), % verifica se existem stacks entre a posição inicial e final
			\+ checkEmptyCell(Board, RowTo, ColumnTo),! % verifica se a posição final tem alguma peça para ser capturada
		);
		fail
	).
