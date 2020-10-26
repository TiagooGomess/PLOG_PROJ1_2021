
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
