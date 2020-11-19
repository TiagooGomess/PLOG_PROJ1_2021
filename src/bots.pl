:-ensure_loaded('utils.pl').
:-ensure_loaded('board.pl').
:-ensure_loaded('game.pl').

:- use_module(library(between)).

getMoveEasy(GameState, Player, RowStart, ColumnStart, RowEnd):-
    getPiecePosFrom(Board, Player, RowStart, ColumnStart),
	getPiecePosTo(Board, RowStart, ColumnStart, RowEnd, ColumnEnd).

getRowAndColumn(Board, Player,Row,Column):-
    between(0,5,Row),
    between(0,5,Column),
    getPieceByRowAndColumn(Board, Row, Column, Piece),
    Piece = Player.

% pergunta ao jogador qual a posição da peça que quer mover e verifica se é válida
getPiecePosFrom(Board, Player, Row, Column):-
	repeat,
    getRowAndColumn(Board, Player,Row,Column),
    nl,write('Row:'),write(Row),nl,
    write('Column:'),write(Column),nl,
	getPieceByRowAndColumn(Board, Row, Column, Piece),
	(
		(
			Piece = Player, % verifica se a stack que o jogador quer mover lhe pertence
			\+ checkIfStackCannotCapture(Board, Row, Column),!
		);
		fail
	).
	
% pergunta ao jogador qual a posição para onde quer mover a peça e verifica se é válida
getPiecePosTo(Board, RowFrom, ColumnFrom, RowTo, ColumnTo):-
	repeat,
	getRowAndColumn(Board, Player,Row,Column),
	(
		(
			checkOrthogonality(RowFrom, ColumnFrom, RowTo, ColumnTo), % verifica se o movimento é feito ortogonalmente
			checkStacksBetween(Board, RowFrom, ColumnFrom, RowTo, ColumnTo), % verifica se existem stacks entre a posição inicial e final
			\+ checkEmptyCell(Board, RowTo, ColumnTo),! % verifica se a posição final tem alguma peça para ser capturada
		);
		fail
	).
