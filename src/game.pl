:-ensure_loaded('utils.pl').
:-ensure_loaded('bots.pl').

% Escreve no ecrã quem é o jogador atual
printPlayer(0):-
    nl,nl,
    write('White player\'s turn.'),nl,nl.
printPlayer(1):-
    nl,nl,
    write('Black player\'s turn.'),nl,nl.
printPlayer(_).

% peças a serem dispostas no tabuleiro inicialmente, dependendo do tamanho
% 9 pretas, 9 brancas e 18 verdes (tabuleiro 6 x 6)
initialPieces(6,[2,2,1,2,0,2,1,2,0,2,2,1,0,2,1,0,2,2,2,0,1,0,2,0,2,1,2,1,2,0,2,1,2,0,2,1]).
% 20 brancas, 20 pretas e 41 verdes (tabuleiro 9 x 9)
initialPieces(9,[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2]).

% cria o tabuleiro inicial, com um tamanho BoardSize x BoardSize;
% as peças são dispostas no tabuleiro de forma aleatória.
initial(GameState, BoardSize):-
    init_random_state, % muda a seed do random, para termos tabuleiros diferentes de cada vez que iniciamos o jogo
    initialPieces(BoardSize,Pieces),
    %GameState = [ [[1,1,0,2,0,2,2,2,2,2,2,2,2,2,2,2],[2],[3],[3],[3],[3]], [[3],[3],[3],[1,2,0,0,2,0],[3],[3]], [[3],[1,1,2,1,2,2],[3],[3],[3],[3]], [[3],[3], [1,2,2,2,2,0,2],[3],[3],[3]], [[3],[3],[3],[3],[3],[0]], [[3],[3],[3],[3],[1,2,2,2,0,1],[3]] ],
    %GameState = [ [[1],[3],[2],[3],[1],[3]], [[1],[3],[3],[2],[3],[3]], [[3],[3],[3],[3],[3],[2]], [[3],[3], [3],[3],[3],[3]], [[3],[3],[3],[3],[3],[3]], [[3],[3],[3],[2],[3],[0]] ].
    createBoard(GameState, BoardSize, Pieces).
 
% Mostra o tabuleiro de jogo e o jogador atual.
display_game(GameState, Player, Size):-
    clearScreen,
    printHeader,nl,nl,
    row_numbers(Rows,Size), % Rows é uma lista com o número das linhas a ser usada no display do tabuleiro
	printBoard(GameState, Rows, Size),
	printPlayer(Player),
    (
        Player = 2 -> !;
        value(GameState,Player,Points,Size),
        write('Your current score is '),write(Points),write('.'),nl,nl
    ).
    
% determina quem é o próximo jogador
next_player(0, 1).
next_player(1, 0).

% pergunta ao jogador que peça quer mover e para que posição;
% pergunta sempre até ao jogador inserir posições iniciais e finais válidas
askMove(Board, Player, RowStart, ColumnStart, RowEnd, ColumnEnd, Size):-
	nl,write('========================================'),nl,
	askForPiecePosFrom(Board, Player, RowStart, ColumnStart, Size),nl,
	askForPiecePosTo(Board, RowStart, ColumnStart, RowEnd, ColumnEnd, Size),
	nl,nl,write('========================================'),nl,nl.

% move a stack da posição (ColumnStart,RowStart) para (ColumnEnd,RowEnd)
move(Board, NewBoad, RowStart, ColumnStart, RowEnd, ColumnEnd):-
	getStackByRowAndColumn(Board, RowStart, ColumnStart, StackStart),
	append_stack(Board, RowEnd, ColumnEnd, StackStart, NewBoad0),
	clear_cell(NewBoad0, RowStart, ColumnStart, NewBoad).

% sucede quando os dois jogadores passam a jogada sucessivamente (Succession = 2)
game_over(GameState, Sucession, Size):- 
	Sucession = 2,
    display_game(GameState, 2, Size), % Player é 2, para não fazer display do player atual, já que ninguém é a jogar
    nl,nl,nl,write('Game Over!'),nl,nl,nl,
    checkWinner(GameState,Size).

% sucede quando o jogador não pode fazer nenhum movimento
playerPassTheTurn(Board, Player, Size):-
	\+ checkIfPlayerCanMakeMove(Board,Player,Size).

% verifica se uma stack não pode capturar outras, ou seja, se não há nenhuma stack
% na mesma linha ou na mesma coluna
checkIfStackCannotCapture(Board, Row, Column):-
	nth0(Row,Board,RowList),
	checkIfEmptyUnless(RowList,Column),
	getColumnN(Board,Column,ColumnList),
	checkIfEmptyUnless(ColumnList,Row).

% verifica se o Player consegue fazer algum movimento, ou seja, 
% se tem pelo menos uma stack que tenha outra stack na mesma linha ou coluna
checkIfPlayerCanMakeMove(Board,Player,Size):-
	checkIfPlayerCanMakeMove(Board,Board,Player,Size,Size).
checkIfPlayerCanMakeMove([],_,_,0,_):-fail.
checkIfPlayerCanMakeMove([Line|T],Board,Player,Row,Size):-
	Row > 0,
	Row1 is Row - 1,
	(
		RealRow is Size - Row,
		checkIfPlayerCanMakeMoveRow(Board,Line,Player,RealRow,Size,Size) -> !;
		checkIfPlayerCanMakeMove(T,Board,Player,Row1,Size)
	).
	
% verifica se o Player consegue fazer algum movimento,
% para as stacks que tem numa certa linha
checkIfPlayerCanMakeMoveRow(_,[],_,_,0,_):-fail.
checkIfPlayerCanMakeMoveRow(Board,[[H|_]|T],Player,Row,Column,Size):-
	Column > 0,
	Column1 is Column - 1,
	(
		H = Player -> (
			RealColumn is Size - Column,
			\+ checkIfStackCannotCapture(Board, Row, RealColumn) -> !;
			fail
		);
		checkIfPlayerCanMakeMoveRow(Board,T,Player,Row,Column1,Size)
	).

% conta o número de pontos de um dado jogador.
value(Board, Player, Points,Size):-
	value(Board, Player, Points, 0, Size).
value(_, _, Points, Points, 0).
value(Board, Player, N, Points, Row):-
	Row > 0,
	Row1 is Row - 1,
	nth0(Row1, Board, RowList),
	countRowPoints(Player, RowList, Counter),
	Points1 is Points + Counter,
	value(Board, Player, N, Points1, Row1).

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

% dá-nos a altura da pirâmide mais alta do jogador
getHighestStackHeight(Board, Player, Height, Size):-
    getHighestStackHeight(Board,Player,Height,0,Size).
getHighestStackHeight(_,_,Height,Height,0,_).
getHighestStackHeight(Board,Player,N,Height,Row,Size):-
    Row > 0,
    Row1 is Row - 1,
    nth0(Row1,Board,RowList),
    getHighestStackHeightRow(Player,RowList,Height0),
    Height1 is max(Height,Height0),
    getHighestStackHeight(Board,Player,N,Height1,Row1,Size).

% dá-nos a altura da pirâmide mais alta do jogador, numa certa linha
getHighestStackHeightRow(Player,RowList,Height):-
    getHighestStackHeightRow(Player,RowList,Height,0).
getHighestStackHeightRow(_,[],Height,Height).
getHighestStackHeightRow(Player,[[H|T0]|T],N,Height):-
    (
        H = Player -> (
            length([H|T0],Height0),
            Height1 is max(Height,Height0)
        );
        Height1 is Height
    ),
    getHighestStackHeightRow(Player,T,N,Height1).

% verifica e imprime no ecrã quem foi o jogador vencedor
checkWinner(Board, Size):-
	value(Board, 0, WhitePoints, Size),
	value(Board, 1, BlackPoints, Size),
	write('========================================'),nl,nl,
	nl,nl,write('--> Final Score:'),nl,nl,
	write('Black Player: '), write(BlackPoints), write(' points.'),nl,nl,
	write('White Player: '), write(WhitePoints), write(' points.'),nl,nl,
	(
		WhitePoints > BlackPoints -> nl,nl,write('White Player won!'),nl,nl,nl;
		(
			BlackPoints > WhitePoints -> nl,nl,write('Black Player won!'),nl,nl,nl;
            getHighestStackHeight(GameState, 0, HeightWhite),
            getHighestStackHeight(GameState, 1, HeightBlack),
            (
                HeightWhite > HeightBlack -> (
                    nl,nl,write('White Player won, because he has de highest stack!'),nl,nl,nl
                );
                (
                    HeightBlack > HeightWhite -> (
                        nl,nl,write('Black Player won, because he has de highest stack!'),nl,nl,nl
                    );
                    (
                        nl,nl,write('It was a tie! You have the same number of points and the same highest stack height! Please play again!'),nl,nl,nl
                    )
                    
                )
            )
		)
	).

describeBotMove(RStart, CStart, REnd, CEnd,BotLevel):-
    translate_column(ColumnStart,CStart),
    translate_column(ColumnEnd,CEnd),
    translate_row(RowStart,RStart),
    translate_row(RowEnd,REnd),
    nl,nl,
    write(BotLevel),
    write(' bot '),
    write('moving stack from ('),
    write(ColumnStart),
    write(','),
    write(RowStart),
    write(') to ('),
    write(ColumnEnd),
    write(','),
    write(RowEnd),
    write(').'),nl,nl,
    write('========================================'),nl,nl.


% ciclo do jogo; o terceiro argumento, Succession, é 0 se a jogada anterior não teve que ser passada à frente (pass turn),
% ou 1 caso contrário; quando for 2, o jogo termina, porque os jogadores tiveram que passar as suas jogadas sucessivamente.
game_loop(GameState, Player, GameMode, SleepTime, Size):-
    game_loop(GameState, Player, 0, GameMode, SleepTime, Size).
game_loop(GameState, Player, Sucession, GameMode, SleepTime, Size):-
    game_over(GameState, Sucession, Size) -> !;
    (   
        display_game(GameState, Player, Size),
        (
            playerPassTheTurn(GameState, Player,Size) -> (
                write('\nYou need to pass your turn!'),nl,sleep(1),
                nl,write('========================================'),nl,nl,
                NewBoard = GameState,
                Sucession1 is Sucession + 1
            );
            (

                ( % computer vs computer mode
                    (
                        GameMode = 'BotEasyVsBotEasy' -> (
                            choose_move(GameState, Player, 'Easy', RowStart, ColumnStart, RowEnd, ColumnEnd, Size), describeBotMove(RowStart, ColumnStart, RowEnd, ColumnEnd, 'Easy'), sleepBot(SleepTime)
                        );
                        GameMode = 'BotEasyVsBotHard' -> (
                            Player = 1 -> getMoveEasy(GameState, Player, RowStart, ColumnStart, RowEnd, ColumnEnd, Size), describeBotMove(RowStart, ColumnStart, RowEnd, ColumnEnd, 'Easy'), sleepBot(SleepTime);
                            choose_move(GameState, Player, 'Hard', RowStart, ColumnStart, RowEnd, ColumnEnd, Size), describeBotMove(RowStart, ColumnStart, RowEnd, ColumnEnd, 'Hard'), sleepBot(SleepTime)
                        );
                        GameMode = 'BotEasyVsBotDumb' -> (
                            Player = 1 -> getMoveEasy(GameState, Player, RowStart, ColumnStart, RowEnd, ColumnEnd, Size), describeBotMove(RowStart, ColumnStart, RowEnd, ColumnEnd, 'Easy'), sleepBot(SleepTime);
                            choose_move(GameState, Player, 'Dumb', RowStart, ColumnStart, RowEnd, ColumnEnd, Size), describeBotMove(RowStart, ColumnStart, RowEnd, ColumnEnd, 'Dumb'), sleepBot(SleepTime)
                        );
                        GameMode = 'BotHardVsBotHard' -> (
                            choose_move(GameState, Player, 'Hard', RowStart, ColumnStart, RowEnd, ColumnEnd, Size), describeBotMove(RowStart, ColumnStart, RowEnd, ColumnEnd, 'Hard'), sleepBot(SleepTime)
                        );
                        GameMode = 'BotHardVsBotDumb' -> (
                            Player = 1 -> choose_move(GameState, Player, 'Hard', RowStart, ColumnStart, RowEnd, ColumnEnd, Size), describeBotMove(RowStart, ColumnStart, RowEnd, ColumnEnd, 'Hard'), sleepBot(SleepTime);
                            choose_move(GameState, Player, 'Dumb', RowStart, ColumnStart, RowEnd, ColumnEnd, Size), describeBotMove(RowStart, ColumnStart, RowEnd, ColumnEnd, 'Dumb'), sleepBot(SleepTime)
                        );
                        GameMode = 'BotDumbVsBotDumb' -> (
                            choose_move(GameState, Player, 'Dumb', RowStart, ColumnStart, RowEnd, ColumnEnd, Size), describeBotMove(RowStart, ColumnStart, RowEnd, ColumnEnd, 'Dumb'), sleepBot(SleepTime)
                        )
                    ),
                    move(GameState, NewBoard, RowStart, ColumnStart, RowEnd, ColumnEnd),
                    Sucession1 is 0
                );
                ( % player vs player ou player vs computer modes
                    (
                        Player = 0 -> (
                        GameMode = 'PlayerVsPlayer' -> askMove(GameState, Player, RowStart, ColumnStart, RowEnd, ColumnEnd, Size);
                        GameMode = 'PlayerVsBotEasy' -> choose_move(GameState, Player, 'Easy', RowStart, ColumnStart, RowEnd, ColumnEnd, Size), describeBotMove(RowStart, ColumnStart, RowEnd, ColumnEnd,'Easy'), sleepBot(SleepTime);
                        GameMode = 'PlayerVsBotHard' -> choose_move(GameState, Player, 'Hard', RowStart, ColumnStart, RowEnd, ColumnEnd, Size), describeBotMove(RowStart, ColumnStart, RowEnd, ColumnEnd,'Hard'), sleepBot(SleepTime);
                        GameMode = 'PlayerVsBotDumb' -> choose_move(GameState, Player, 'Dumb', RowStart, ColumnStart, RowEnd, ColumnEnd, Size), describeBotMove(RowStart, ColumnStart, RowEnd, ColumnEnd,'Dumb'), sleepBot(SleepTime);
                        nl,nl,nl,write('Invalid Game Mode!!!'),nl,nl,nl,fail
                        );
                        askMove(GameState, Player, RowStart, ColumnStart, RowEnd, ColumnEnd, Size)
                    ),
                    move(GameState, NewBoard, RowStart, ColumnStart, RowEnd, ColumnEnd),
                    Sucession1 is 0   
                )
            )
        ),
        next_player(Player, NextPlayer),
        game_loop(NewBoard, NextPlayer, Sucession1, GameMode, SleepTime, Size)
    ).
    