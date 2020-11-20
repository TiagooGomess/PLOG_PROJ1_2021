:-use_module(library(system)).

:-ensure_loaded('utils.pl').
:-ensure_loaded('board.pl').
:-ensure_loaded('game.pl').
:-ensure_loaded('play.pl').


printMainMenu:-
	clearScreen,
	printHeader,
	write('=                                ='), nl,
	write('=   1. Play                      ='), nl,
	write('=   2. How to play               ='), nl,
	write('=   3. Exit                      ='), nl,
	write('=                                ='), nl,
	write('=================================='), nl,
	write('Choose an option:'), nl.

mainMenu:-
	printMainMenu,
    getChar(Input),
	(
        Input = '1' -> gameModeMenu, playAgain;
        Input = '2' -> howToPlay, play; % entra em howToPlay e volta para play.
        Input = '3';

		nl, write('Invalid input!'), nl,
		pressEnterToContinue,nl,
        play
    ).

gameModeMenu:-
	printGameModeMenu,
	getChar(Input),
	(
		Input = '1' -> clearScreen, playerVsPlayer;
		Input = '2' -> clearScreen, playerVsBotMenu;
		Input = '3' -> clearScreen, botVsBot;
		Input = '4';

		nl,write('Invalid input!'),nl,
		pressEnterToContinue,nl,
		gameModeMenu
	).

printGameModeMenu:-
	clearScreen,
	write('================================='), nl,
	write('=      :::: Game Mode ::::      ='), nl,
	write('================================='), nl,
	write('=                               ='), nl,
	write('=   1. Player vs. Player        ='), nl,
	write('=   2. Player vs. Computer      ='), nl,
	write('=   3. Computer vs. Computer    ='), nl,
	write('=   4. Back                     ='), nl,
	write('=                               ='), nl,
	write('================================='), nl,
	write('Choose an option:'), nl.

playerVsBotMenu:-
	printPlayerVsBotMenu,
	getChar(Input),
	(
		Input = '1' -> clearScreen, playerVsBotEasy;
		Input = '2' -> clearScreen, playerVsBotHard;
		Input = '3';

		nl,
		nl,write('Invalid input!'),nl,
		pressEnterToContinue, nl,
		playerVsBotMenu
	).

printPlayerVsBotMenu:-
	clearScreen,
	write('================================='), nl,
	write('=    :::: Bot Difficulty ::::   ='), nl,
	write('================================='), nl,
	write('=                               ='), nl,
	write('=   1. Easy                     ='), nl,
	write('=   2. Hard                     ='), nl,
	write('=   3. Back                     ='), nl,
	write('=                               ='), nl,
	write('================================='), nl,
	write('Choose an option:'), nl.


playerVsPlayer:-
	clearScreen,
	printHeader,nl,nl,nl,
	initial(GameState),
	game_loop(GameState, 1, 'PlayerVsPlayer').

botVsBot:-
	clearScreen,
	write('Comming soon!'),nl,
	sleep(1),
	mainMenu.

playerVsBotEasy:-
	clearScreen,
	printHeader,nl,nl,nl,
	initial(GameState),
	game_loop(GameState, 1, 'PlayerVsBotEasy').

playerVsBotHard:-
	clearScreen,
	write('Comming soon!'),nl,
	sleep(1),
	mainMenu.

playAgain:-
	printPlayAgain,
	getChar(Input),
	(
		Input = '1' -> clearScreen, mainMenu;
		Input = '2';

		nl,
		nl,write('Invalid input!'),nl,
		pressEnterToContinue, nl,
		playAgain
	).

printPlayAgain:-
	nl,nl,nl,nl,
	write('================================='), nl,
	write('=    ::::   Play Again?  ::::   ='), nl,
	write('================================='), nl,
	write('=                               ='), nl,
	write('=   1. Yes!                     ='), nl,
	write('=   2. No!                      ='), nl,
	write('=                               ='), nl,
	write('================================='), nl,
	write('Choose an option:'), nl.
	
howToPlay:-
	clearScreen,
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
	write('==============================================================='), nl,
	pressEnterToContinue, nl.
		
