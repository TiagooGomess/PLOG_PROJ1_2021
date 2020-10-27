:-ensure_loaded('utils.pl').

% Escreve no ecrã quem é o jogador atual
printPlayer(0):-
    nl,nl,
    write('White player\'s turn.'),nl,nl.
printPlayer(1):-
    nl,nl,
    write('Black player\'s turn.'),nl,nl.
printPlayer(Player) :- 
    Player \= 0,
    Player \= 1,
    notValidPlayer.
notValidPlayer:-
    nl,nl,
    write('The player number is not valid! It needs to be 0 or 1!'),nl,nl.

% peças a serem dispostas no tabuleiro inicialmente
initialPieces([2,2,1,2,0,2,1,2,0,2,2,1,0,2,1,0,2,2,2,0,1,0,2,0,2,1,2,1,2,0,2,1,2,0,2,1]).

% cria o tabuleiro inicial, de tamanho 6 x 6, com 9 pirâmides brancas (0), 
% 9 pirâmides pretas (1) e 18 pirâmides verdes (2).
% as peças são dispostas no tabuleiro de forma aleatória.
initial(GameState):-
    initialPieces(Pieces),
	createBoard(GameState, 6, Pieces).

% Mostra o tabuleiro de jogo e o jogador atual.
display_game(GameState, Player):-
    row_numbers(Rows), % Rows é uma lista com o número das linhas a ser usada no display do tabuleiro
	printBoard(GameState, Rows),
	printPlayer(Player).
