:- use_module(library(random)).
:- use_module(library(between)).
:- use_module(library(lists)).

:-ensure_loaded('utils.pl').
:-ensure_loaded('board.pl').
:-ensure_loaded('game.pl').

% obtém um movimento, na forma [RowFrom, ColumnFrom, RowTo, ColumnTo]
getRandomMove(RowFrom, ColumnFrom, RowTo, ColumnTo):-
	between(0,5,RowFrom),
    between(0,5,ColumnFrom),
	between(0,5,RowTo),
    between(0,5,ColumnTo).

% obtém todos os movimentos válidos na forma [RowFrom, ColumnFrom, RowTo, ColumnTo]
getAllValidMoves(Board,Player,AllMoves):-
	findall(
        [RowFrom, ColumnFrom, RowTo, ColumnTo],
        (
            getRandomMove(RowFrom, ColumnFrom, RowTo, ColumnTo),
			getPieceByRowAndColumn(Board, RowFrom, ColumnFrom, Piece),
   	 		Piece = Player, % verifica se a stack pertence ao jogador atual
			\+ checkIfStackCannotCapture(Board, RowFrom, ColumnFrom), % verifica se a stack pode capturar outras stacks
			checkOrthogonality(RowFrom, ColumnFrom, RowTo, ColumnTo), % verifica se o movimento é feito ortogonalmente
			checkStacksBetween(Board, RowFrom, ColumnFrom, RowTo, ColumnTo), % verifica se existem stacks entre a posição inicial e final
			\+ checkEmptyCell(Board, RowTo, ColumnTo) % verifica se a posição final tem alguma peça para ser capturada
		),
    	AllMoves
	).

% este predicado dá-nos um movimento aleatório válido
getMoveEasy(GameState, Player, RowStart, ColumnStart, RowEnd, ColumnEnd):-
	getAllValidMoves(GameState, Player, AllMoves),
	random_member(Move,AllMoves),
	nth0(0,Move,RowStart),
	nth0(1,Move,ColumnStart),
	nth0(2,Move,RowEnd),
	nth0(3,Move,ColumnEnd).