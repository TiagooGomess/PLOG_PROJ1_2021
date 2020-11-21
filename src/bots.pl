:- use_module(library(random)).
:- use_module(library(between)).
:- use_module(library(lists)).

:-ensure_loaded('utils.pl').
:-ensure_loaded('board.pl').
:-ensure_loaded('game.pl').

% obtém um movimento, na forma [RowFrom, ColumnFrom, RowTo, ColumnTo]
getRandomMove(RowFrom, ColumnFrom, RowTo, ColumnTo,Size):-
	S is Size - 1,
	between(0,S,RowFrom),
    between(0,S,ColumnFrom),
	between(0,S,RowTo),
    between(0,S,ColumnTo).

% obtém todos os movimentos válidos na forma [RowFrom, ColumnFrom, RowTo, ColumnTo]
valid_moves(Board,Player,AllMoves,Size):-
	findall(
        [RowFrom, ColumnFrom, RowTo, ColumnTo],
        (
            getRandomMove(RowFrom, ColumnFrom, RowTo, ColumnTo,Size),
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
getMoveEasy(GameState, Player, RowStart, ColumnStart, RowEnd, ColumnEnd, Size):-
	valid_moves(GameState, Player, AllMoves,Size),
	random_member(Move,AllMoves),
	nth0(0,Move,RowStart),
	nth0(1,Move,ColumnStart),
	nth0(2,Move,RowEnd),
	nth0(3,Move,ColumnEnd).

% adiciona a pontuação que o movimento vai proporcionar;
% MoveWithScore fica na forma [MoveScore,[RowStart, ColumnStart, RowEnd, ColumnEnd]],
% para ser mais fácil ordenar os movimentos por score
addMoveScore(GameState, Player, Move, MoveWithScore, Size):-
	value(GameState, Player, PointsBefore, Size),
	nth0(0,Move,RowStart),
	nth0(1,Move,ColumnStart),
	nth0(2,Move,RowEnd),
	nth0(3,Move,ColumnEnd),
	move(GameState, NewGameState, RowStart, ColumnStart, RowEnd, ColumnEnd),
	value(NewGameState, Player, PointsAfter, Size),
	MoveScore is PointsAfter - PointsBefore,
	MoveWithScore = [MoveScore,Move].

% aplica o predicado addMoveScore a todos os movimentos
addScoreToMoves(GameState,Player,AllMoves,AllMovesWithScore,Size):-
	addScoreToMoves(GameState,Player,AllMoves,AllMovesWithScore,[],Size).
addScoreToMoves(_,_,[],AllMovesWithScore,AllMovesWithScore,_).
addScoreToMoves(GameState,Player,[Move|T],M,AllMovesWithScore,Size):-
	addMoveScore(GameState,Player,Move,MoveWithScore,Size),
	addScoreToMoves(GameState,Player,T,M,[MoveWithScore|AllMovesWithScore],Size).

% este predicado dá-nos o melhor movimento possível, ou seja, o que dá a melhor pontuação
getMoveHard(GameState, Player, RowStart, ColumnStart, RowEnd, ColumnEnd,Size):-
	valid_moves(GameState, Player, AllMoves, Size),
	addScoreToMoves(GameState, Player, AllMoves, AllMovesWithScore,Size),
	sort(AllMovesWithScore,AllMovesWithScoreSorted0),
	reverse(AllMovesWithScoreSorted0,AllMovesWithScoreSorted),
	nth0(0,AllMovesWithScoreSorted,MoveWithScore),
	nth0(1,MoveWithScore,Move),
	nth0(0,Move,RowStart),
	nth0(1,Move,ColumnStart),
	nth0(2,Move,RowEnd),
	nth0(3,Move,ColumnEnd).

% este predicado dá-nos o pior movimento possível, ou seja, o que dá a pior pontuação
getMoveDumb(GameState, Player, RowStart, ColumnStart, RowEnd, ColumnEnd,Size):-
	valid_moves(GameState, Player, AllMoves, Size),
	addScoreToMoves(GameState, Player, AllMoves, AllMovesWithScore,Size),
	sort(AllMovesWithScore,AllMovesWithScoreSorted),
	nth0(0,AllMovesWithScoreSorted,MoveWithScore),
	nth0(1,MoveWithScore,Move),
	nth0(0,Move,RowStart),
	nth0(1,Move,ColumnStart),
	nth0(2,Move,RowEnd),
	nth0(3,Move,ColumnEnd).

choose_move(GameState,Player,Level,RowStart, ColumnStart, RowEnd, ColumnEnd, Size):-
	(
		Level = 'Easy' -> getMoveEasy(GameState, Player, RowStart, ColumnStart, RowEnd, ColumnEnd, Size);
		Level = 'Hard' -> getMoveHard(GameState, Player, RowStart, ColumnStart, RowEnd, ColumnEnd, Size);
		Level = 'Dumb' -> getMoveDumb(GameState, Player, RowStart, ColumnStart, RowEnd, ColumnEnd, Size)
	).
