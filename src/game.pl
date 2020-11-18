:-ensure_loaded('utils.pl').

% Escreve no ecrã quem é o jogador atual
printPlayer(0):-
    nl,nl,
    write('White player\'s turn.'),nl,nl.
printPlayer(1):-
    nl,nl,
    write('Black player\'s turn.'),nl,nl.
printPlayer(_).

% peças a serem dispostas no tabuleiro inicialmente
initialPieces([2,2,1,2,0,2,1,2,0,2,2,1,0,2,1,0,2,2,2,0,1,0,2,0,2,1,2,1,2,0,2,1,2,0,2,1]).

% cria o tabuleiro inicial, de tamanho 6 x 6, com 9 pirâmides brancas (0), 
% 9 pirâmides pretas (1) e 18 pirâmides verdes (2).
% as peças são dispostas no tabuleiro de forma aleatória.
initial(GameState):-
    init_random_state, % muda a seed do random, para termos tabuleiros diferentes de cada vez que iniciamos o jogo
    initialPieces(Pieces),
    %GameState = [ [[1,1,0,2,0,2,2,2,2,2,2,2,2,2,2,2],[2],[3],[3],[3],[3]], [[3],[3],[3],[1,2,0,0,2,0],[3],[3]], [[3],[1,1,2,1,2,2],[3],[3],[3],[3]], [[3],[3], [1,2,2,2,2,0,2],[3],[3],[3]], [[3],[3],[3],[3],[3],[0]], [[3],[3],[3],[3],[1,2,2,2,0,1],[3]] ].
    createBoard(GameState, 6, Pieces).
 
% Mostra o tabuleiro de jogo e o jogador atual.
display_game(GameState, Player):-
    clearScreen,
    printHeader,nl,nl,
    row_numbers(Rows), % Rows é uma lista com o número das linhas a ser usada no display do tabuleiro
	printBoard(GameState, Rows),
	printPlayer(Player),
    (
        Player = 2 -> !;
        countPlayerPoints(GameState,Player,Points),
        write('Your current score is '),write(Points),write('.'),nl,nl
    ).
    
% determina quem é o próximo jogador
next_player(0, 1).
next_player(1, 0).

% pergunta ao jogador que peça quer mover e para que posição;
% pergunta sempre até ao jogador inserir posições iniciais e finais válidas
askMove(Board, Player, RowStart, ColumnStart, RowEnd, ColumnEnd):-
	nl,write('========================================'),nl,
	askForPiecePosFrom(Board, Player, RowStart, ColumnStart),nl,
	askForPiecePosTo(Board, RowStart, ColumnStart, RowEnd, ColumnEnd),
	nl,nl,write('========================================'),nl,nl.

% move a stack da posição (ColumnStart,RowStart) para (ColumnEnd,RowEnd)
makeMove(Board, NewBoad, RowStart, ColumnStart, RowEnd, ColumnEnd):-
	getStackByRowAndColumn(Board, RowStart, ColumnStart, StackStart),
	append_stack(Board, RowEnd, ColumnEnd, StackStart, NewBoad0),
	clear_cell(NewBoad0, RowStart, ColumnStart, NewBoad).

% sucede quando os dois jogadores passam a jogada sucessivamente (Succession = 2)
checkEnd(Sucession):- 
	Sucession = 2.

% sucede quando o jogador não pode fazer nenhum movimento
playerPassTheTurn(Board, Player):-
	\+ checkIfPlayerCanMakeMove(Board,Player).

% verifica se uma stack não pode capturar outras, ou seja, se não há nenhuma stack
% na mesma linha ou na mesma coluna
checkIfStackCannotCapture(Board, Row, Column):-
	nth0(Row,Board,RowList),
	checkIfEmptyUnless(RowList,Column),
	getColumnN(Board,Column,ColumnList),
	checkIfEmptyUnless(ColumnList,Row).

% verifica se o Player consegue fazer algum movimento, ou seja, 
% se tem pelo menos uma stack que tenha outra stack na mesma linha ou coluna
checkIfPlayerCanMakeMove(Board,Player):-
	checkIfPlayerCanMakeMove(Board,Board,Player,6).
checkIfPlayerCanMakeMove([],_,_,0):-fail.
checkIfPlayerCanMakeMove([Line|T],Board,Player,Row):-
	Row > 0,
	Row1 is Row - 1,
	(
		RealRow is 6 - Row,
		checkIfPlayerCanMakeMoveRow(Board,Line,Player,RealRow,6) -> !;
		checkIfPlayerCanMakeMove(T,Board,Player,Row1)
	).
	
% verifica se o Player consegue fazer algum movimento,
% para as stacks que tem numa certa linha
checkIfPlayerCanMakeMoveRow(_,[],_,_,0):-fail.
checkIfPlayerCanMakeMoveRow(Board,[[H|_]|T],Player,Row,Column):-
	Column > 0,
	Column1 is Column - 1,
	(
		H = Player -> (
			RealColumn is 6 - Column,
			\+ checkIfStackCannotCapture(Board, Row, RealColumn) -> !;
			fail
		);
		checkIfPlayerCanMakeMoveRow(Board,T,Player,Row,Column1)
	).

% conta o número de pontos de um dado jogador.
countPlayerPoints(Board, Player, Points):-
	countPlayerPoints(Board, Player, Points, 0, 6).
countPlayerPoints(_, _, Points, Points, 0).
countPlayerPoints(Board, Player, N, Points, Row):-
	Row > 0,
	Row1 is Row - 1,
	nth0(Row1, Board, RowList),
	countRowPoints(Player, RowList, Counter),
	Points1 is Points + Counter,
	countPlayerPoints(Board, Player, N, Points1, Row1).

% conta o número de pontos de um dado jogador, numa linha.
countRowPoints(Player, RowList, Counter):-
	countRowPoints(Player, RowList, Counter, 0).
countRowPoints(_, [], Counter, Counter).
countRowPoints(Player, [[H|T0]|T], C, Counter):-
	(
		H = Player -> (
            occurrences_of([H|T0],2,Counter0), % conta o número de peças verdes que a stack tem
            Counter1 is Counter + Counter0
        );
		Counter1 is Counter
	),
	countRowPoints(Player, T, C, Counter1).

% verifica e imprime no ecrã quem foi o jogador vencedor
checkWinner(Board):-
	countPlayerPoints(Board, 0, WhitePoints),
	countPlayerPoints(Board, 1, BlackPoints),
	write('========================================'),nl,nl,
	nl,nl,write('--> Final Score:'),nl,nl,
	write('Black Player: '), write(BlackPoints), write(' points.'),nl,nl,
	write('White Player: '), write(WhitePoints), write(' points.'),nl,nl,
	(
		WhitePoints > BlackPoints -> nl,nl,write('White Player won!'),nl,nl,nl;
		(
			BlackPoints > WhitePoints -> nl,nl,write('Black Player won!'),nl,nl,nl;
			write('It was a tie!') % TODO verificar qual o jogador com a stack mais alta 
		)
	).

% ciclo do jogo; o terceiro argumento, Succession, é 0 se a jogada anterior não teve que ser passada à frente (pass turn),
% ou 1 caso contrário; quando for 2, o jogo termina, porque os jogadores tiveram que passar as suas jogadas sucessivamente.
game_loop(GameState, Player):-
    game_loop(GameState, Player, 0).
game_loop(GameState, Player, Sucession):-
    checkEnd(Sucession) -> (
        display_game(GameState, 2, _), % Player é 2, para não fazer display do player atual, já que ninguém é a jogar
        nl,nl,nl,write('Game Over!'),nl,nl,nl,
        checkWinner(GameState),! 
    );
    (   
        display_game(GameState, Player),
        (
            playerPassTheTurn(GameState, Player) -> (
                write('\nYou need to pass your turn!'),nl,
                nl,write('========================================'),nl,nl,
                NewBoard = GameState,
                Sucession1 is Sucession + 1
            );
            (
                askMove(GameState, Player, RowStart, ColumnStart, RowEnd, ColumnEnd),
                makeMove(GameState, NewBoard, RowStart, ColumnStart, RowEnd, ColumnEnd),
                Sucession1 is 0
            )
        ),
        next_player(Player, NextPlayer),
        game_loop(NewBoard, NextPlayer,Sucession1)
    ).
    