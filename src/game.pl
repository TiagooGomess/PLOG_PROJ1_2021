:-ensure_loaded('utils.pl').

% Escreve no ecrã quem é o jogador atual
printPlayer(0):-
    nl,nl,
    write('White player\'s turn.'),nl,nl.
printPlayer(1):-
    nl,nl,
    write('Black player\'s turn.'),nl,nl.
printPlayer(Player).

% peças a serem dispostas no tabuleiro inicialmente
initialPieces([2,2,1,2,0,2,1,2,0,2,2,1,0,2,1,0,2,2,2,0,1,0,2,0,2,1,2,1,2,0,2,1,2,0,2,1]).

% cria o tabuleiro inicial, de tamanho 6 x 6, com 9 pirâmides brancas (0), 
% 9 pirâmides pretas (1) e 18 pirâmides verdes (2).
% as peças são dispostas no tabuleiro de forma aleatória.
initial(GameState):-
    init_random_state, % muda a seed do random, para termos tabuleiros diferentes de cada vez que iniciamos o jogo
    initialPieces(Pieces),
    GameState = [ [[1,1,0,2,0,2,2,2,2,2,2,2,2,2,2,2],[2],[3],[3],[3],[3]], [[3],[3],[3],[1,2,0,0,2,0],[3],[3]], [[3],[1,1,2,1,2,2],[3],[3],[3],[3]], [[3],[3], [1,2,2,2,2,0,2],[3],[3],[3]], [[3],[3],[3],[3],[3],[0]], [[3],[3],[3],[3],[1,2,2,2,0,1],[3]] ].
    %createBoard(GameState, 6, Pieces).
 
% Mostra o tabuleiro de jogo e o jogador atual.
display_game(GameState, Player):-
    row_numbers(Rows), % Rows é uma lista com o número das linhas a ser usada no display do tabuleiro
	printBoard(GameState, Rows),
	printPlayer(Player).

% determina quem é o próximo jogador
next_player(0, 1).
next_player(1, 0).

% ciclo do jogo; o terceiro argumento, Succession, é 0 se a jogada anterior não teve que ser passada à frente (pass turn),
% ou 1 caso contrário; quando for 2, o jogo termina, porque os jogadores tiveram que passar as suas jogadas sucessivamente.
game_loop(GameState, Player):-
    game_loop(GameState, Player, 0).
game_loop(GameState, Player, Sucession):-
    checkEnd(GameState, Player, Sucession) -> (
        display_game(GameState, 2), % Player é 2, para não fazer display do player atual, já que ninguém é a jogar
        nl,nl,nl,write('Game Over!'),nl,nl,nl,! 
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
    
