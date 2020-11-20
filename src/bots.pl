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

% adiciona a pontuação que o movimento vai proporcionar;
% MoveWithScore fica na forma [MoveScore,[RowStart, ColumnStart, RowEnd, ColumnEnd]],
% para ser mais fácil ordenar os movimentos por score
addMoveScore(GameState, Player, Move, MoveWithScore):-
	countPlayerPoints(GameState, Player, PointsBefore),
	nth0(0,Move,RowStart),
	nth0(1,Move,ColumnStart),
	nth0(2,Move,RowEnd),
	nth0(3,Move,ColumnEnd),
	makeMove(GameState, NewGameState, RowStart, ColumnStart, RowEnd, ColumnEnd),
	countPlayerPoints(NewGameState, Player, PointsAfter),
	MoveScore is PointsAfter - PointsBefore,
	MoveWithScore = [MoveScore,Move].

% aplica o predicado addMoveScore a todos os movimentos
addScoreToMoves(GameState,Player,AllMoves,AllMovesWithScore):-
	addScoreToMoves(GameState,Player,AllMoves,AllMovesWithScore,[]).
addScoreToMoves(_,_,[],AllMovesWithScore,AllMovesWithScore).
addScoreToMoves(GameState,Player,[Move|T],M,AllMovesWithScore):-
	addMoveScore(GameState,Player,Move,MoveWithScore),
	addScoreToMoves(GameState,Player,T,M,[MoveWithScore|AllMovesWithScore]).

% este predicado dá-nos o melhor movimento possível, ou seja, o que dá a melhor pontuação
getMoveHard(GameState, Player, RowStart, ColumnStart, RowEnd, ColumnEnd):-
	getAllValidMoves(GameState, Player, AllMoves),
	addScoreToMoves(GameState, Player, AllMoves, AllMovesWithScore),
	sort(AllMovesWithScore,AllMovesWithScoreSorted0),
	reverse(AllMovesWithScoreSorted0,AllMovesWithScoreSorted),
	nth0(0,AllMovesWithScoreSorted,MoveWithScore),
	nth0(1,MoveWithScore,Move),
	nth0(0,Move,RowStart),
	nth0(1,Move,ColumnStart),
	nth0(2,Move,RowEnd),
	nth0(3,Move,ColumnEnd).

% este predicado dá-nos o pior movimento possível, ou seja, o que dá a pior pontuação
getMoveDumb(GameState, Player, RowStart, ColumnStart, RowEnd, ColumnEnd):-
	getAllValidMoves(GameState, Player, AllMoves),
	addScoreToMoves(GameState, Player, AllMoves, AllMovesWithScore),
	sort(AllMovesWithScore,AllMovesWithScoreSorted),
	nth0(0,AllMovesWithScoreSorted,MoveWithScore),
	nth0(1,MoveWithScore,Move),
	nth0(0,Move,RowStart),
	nth0(1,Move,ColumnStart),
	nth0(2,Move,RowEnd),
	nth0(3,Move,ColumnEnd).
