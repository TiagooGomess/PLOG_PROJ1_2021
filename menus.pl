:-ensure_loaded('utils.pl').
:-ensure_loaded('board.pl').

start:-
    printMainMenu,
    getMainMenuOption.

printMainMenu:-
	write('=================================='), nl,
	write('=       :::: Greener ::::        ='), nl,
	write('=================================='), nl,
	write('=                                ='), nl,
	write('=   1. Play                      ='), nl,
	write('=   2. How to play               ='), nl,
	write('=   3. Exit                      ='), nl,
	write('=                                ='), nl,
	write('=================================='), nl,
	write('Choose an option:'), nl.

getMainMenuOption:-
    getChar(Input),(
        Input = '1' -> gameMenu;
        Input = '2' -> helpMenu, start; % entra em helpMenu e volta para start.
        Input = '3';

        nl, write('Invalid input!'), nl,
        start
    ).

gameMenu:-
	createBoard(Board, 6, [2,2,1,2,0,2,1,2,0,2,2,1,0,2,1,0,2,2,2,0,1,0,2,0,2,1,2,1,2,0,2,1,2,0,2,1]),
	write('Created Board'), nl,
	write('---------------------------------------'),nl,
	printBoard(Board,[6,5,4,3,2,1]), nl,
	write('---------------------------------------'),nl,
	write('Done!').
	
helpMenu:-
	write('==============================================================='), nl,
	write('=     :::: How To Play ::::                                   ='), nl,
	write('==============================================================='), nl,
	write('='), nl,
	write('=   Definitions:'), nl,
	write('=   1. A stack is either one pyramid or several pyramids'), nl,
	write('=   stacked on top of each other.'), nl,
	write('=   2. A stack is controlled by the color of the topmost'), nl,
	write('=   pyramid. So a Black stack is a stack of any height'), nl,
	write('=   with a black pyramid on top, and so on...'), nl,
	write('=   '), nl,
	write('=   -> Greener is a game for two players (Black and White).'), nl,
	write('=   -> There are 9 black (1) pyramids, 9 white (0) and'), nl,
	write('=   18 green (2).'), nl,
	write('=   -> First, the pyramids are randomly places in the board.'), nl,
	write('=   -> Each player has an allocated color (Black or White).'), nl,
	write('=   -> Green is a neutral color.'), nl,
	write('=   -> Black starts; players alternate turns during the game,'), nl,
	write('=   until both players pass in sucession.'), nl,
	write('=   -> On your turn, you must make on capture if possible;'), nl,
	write('=   otherwise, you pass the turn.'), nl,
	write('=   -> Stacks capture other stacks that are on the same row'), nl,
	write('=   or column and with no other stacks in between them;'), nl,
	write('=   stacks cannot be split.'), nl,
	write('=   -> You can capture stacks of any colour (even your own).'), nl,
	write('=   -> The game ends when the players pass in sucession.'), nl,
	write('=   -> The player with the most green pyramids captured'), nl,
	write('=   (being part of their control) wins the game.'), nl,
	write('=   -> In the case of a tie, the player with the highest'), nl,
	write('=   stacks wins.'), nl,
	write('=   -> If the tie persists, play again.'), nl,
	write('=   '), nl,
	write('=   Strategy:'), nl,
	write('=   -> Capturing opponents pyramids is a good strategy, but'), nl,
	write('=   each time you do so, you are not capturing green ones!'), nl,
	write('=   -> Try to find the perfect balance between the two.'), nl,
	write('=   '), nl,
	write('==============================================================='), nl.
		
