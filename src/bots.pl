:-ensure_loaded('utils.pl').
:-ensure_loaded('board.pl').
:-ensure_loaded('game.pl').

:- use_module(library(between)).

getMoveEasy(GameState, Player, RowStart, ColumnStart, RowEnd, ColumnEnd):-
    getPiecePosFrom(GameState, Player, RowStart, ColumnStart).
	getPiecePosTo(GameState, RowStart, ColumnStart, RowEnd, ColumnEnd).

getRandomRowAndColumn(Row,Column):-
    between(0,5,Row),
    between(0,5,Column).

getPiecePosFrom(Board, Player, Row, Column):-
	repeat,
	getRandomRowAndColumn(Row,Column),
	(
		(
			getPieceByRowAndColumn(Board, Row, Column, Piece),
   	 		Piece = Player,
			\+ checkIfStackCannotCapture(Board, Row, Column),!
		);
		fail
	).
	
getPiecePosTo(Board, RowFrom, ColumnFrom, RowTo, ColumnTo):-
	repeat,
	getRandomRowAndColumn(Row,Column),
	(
		(
			checkOrthogonality(RowFrom, ColumnFrom, RowTo, ColumnTo), % verifica se o movimento é feito ortogonalmente
			checkStacksBetween(Board, RowFrom, ColumnFrom, RowTo, ColumnTo), % verifica se existem stacks entre a posição inicial e final
			\+ checkEmptyCell(Board, RowTo, ColumnTo),! % verifica se a posição final tem alguma peça para ser capturada
		);
		fail
	).
	