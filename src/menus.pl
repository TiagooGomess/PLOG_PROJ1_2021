:-use_module(library(system)).

:-ensure_loaded('utils.pl').
:-ensure_loaded('board.pl').
:-ensure_loaded('game.pl').
:-ensure_loaded('play.pl').

% imprime o menu principal
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

% menu principal
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

% menu para escolher o modo de jogo
gameModeMenu:-
	printGameModeMenu,
	getChar(Input),
	(
		Input = '1' -> clearScreen, playerVsPlayer;
		Input = '2' -> clearScreen, playerVsBotMenu;
		Input = '3' -> clearScreen, computerVsComputerMenu;
		Input = '4' -> mainMenu,!;

		nl,write('Invalid input!'),nl,
		pressEnterToContinue,nl,
		gameModeMenu
	).

printBoardSizeMenu:-
	clearScreen,
	write('================================='), nl,
	write('=      :::: Board Size ::::     ='), nl,
	write('================================='), nl,
	write('=                               ='), nl,
	write('=   1. 6 x 6                    ='), nl,
	write('=   2. 9 x 9                    ='), nl,
	write('=   3. Back                     ='), nl,
	write('=                               ='), nl,
	write('================================='), nl,
	write('Choose an option:'), nl.

boardSizeMenu(Size):-
	printBoardSizeMenu,
	getChar(Input),
	(
		Input = '1' -> Size is 6;
		Input = '2' -> Size is 9;
		Input = '3' -> gameModeMenu,!;

		nl,write('Invalid input!'),nl,
		pressEnterToContinue,nl,
		boardSizeMenu(Size)
	).

% imprime o menu do modo de jogo
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

% menu de jogador contra computador
playerVsBotMenu:-
	printPlayerVsBotMenu,
	getChar(Input),
	(
		Input = '1' -> clearScreen, playerVsBotEasy;
		Input = '2' -> clearScreen, playerVsBotHard;
		Input = '3' -> clearScreen, playerVsBotDumb;
		Input = '4' -> gameModeMenu,!;

		nl,
		nl,write('Invalid input!'),nl,
		pressEnterToContinue, nl,
		playerVsBotMenu
	).

% imprime o menu de jogador contra computador
printPlayerVsBotMenu:-
	clearScreen,
	write('================================='), nl,
	write('=    :::: Bot Difficulty ::::   ='), nl,
	write('================================='), nl,
	write('=                               ='), nl,
	write('=   1. Easy                     ='), nl,
	write('=   2. Hard                     ='), nl,
	write('=   3. Dumb                     ='), nl,
	write('=   4. Back                     ='), nl,
	write('=                               ='), nl,
	write('================================='), nl,
	write('Choose an option:'), nl.

% imprime o menu de computador contra computador
printComputerVsComputerMenu:-
	clearScreen,
	write('================================='), nl,
	write('=    ::::  Bots Levels   ::::   ='), nl,
	write('================================='), nl,
	write('=                               ='), nl,
	write('=   1. Easy vs Easy             ='), nl,
	write('=   2. Easy vs Hard             ='), nl,
	write('=   3. Easy vs Dumb             ='), nl,
	write('=   4. Hard vs Hard             ='), nl,
	write('=   5. Hard vs Dumb             ='), nl,
	write('=   6. Dumb vs Dumb             ='), nl,
	write('=   7. Back                     ='), nl,
	write('=                               ='), nl,
	write('================================='), nl,
	write('Choose an option:'), nl.

printBotTimeMenu:-
	clearScreen,
	write('================================='), nl,
	write('=    ::::   Bots Time   ::::    ='), nl,
	write('================================='), nl,
	write('=                               ='), nl,
	write('=   1. 0 seconds                ='), nl,
	write('=   2. 0.1 seconds              ='), nl,
	write('=   3. 0.5 seconds              ='), nl,
	write('=   4. 1 second                 ='), nl,
	write('=   5. 2 seconds                ='), nl,
	write('=   6. 5 seconds                ='), nl,
	write('=   7. Back                     ='), nl,
	write('=                               ='), nl,
	write('================================='), nl,
	write('Choose an option:'), nl.

botTimeMenu(Time):-
	printBotTimeMenu,
	getChar(Input),
	(
		Input = '1' -> Time is 0;
		Input = '2' -> Time is 0.1;
		Input = '3' -> Time is 0.5;
		Input = '4' -> Time is 1;
		Input = '5' -> Time is 2;
		Input = '6' -> Time is 5;
		Input = '7' -> gameModeMenu,!;

		nl,
		nl,write('Invalid input!'),nl,
		pressEnterToContinue, nl,
		botTimeMenu(Time)
	).

% menu de computador contra computador
computerVsComputerMenu:-
	printComputerVsComputerMenu,
	getChar(Input),
	(
		Input = '1' -> clearScreen, botEasyVsBotEasy;
		Input = '2' -> clearScreen, botEasyVsBotHard;
		Input = '3' -> clearScreen, botEasyVsBotDumb;
		Input = '4' -> clearScreen, botHardVsBotHard;
		Input = '5' -> clearScreen, botHardVsBotDumb;
		Input = '6' -> clearScreen, botDumbVsBotDumb;
		Input = '7' -> gameModeMenu,!;

		nl,
		nl,write('Invalid input!'),nl,
		pressEnterToContinue, nl,
		computerVsComputerMenu
	).

% iniciar o jogo bot easy contra bot easy
botEasyVsBotEasy:-
	boardSizeMenu(Size),
	botTimeMenu(Time),
	clearScreen,
	printHeader,nl,nl,nl,
	initial(GameState,Size),
	game_loop(GameState, 1, 'BotEasyVsBotEasy', Time, Size).

% iniciar o jogo bot easy contra bot hard
botEasyVsBotHard:-
	boardSizeMenu(Size),
	botTimeMenu(Time),
	clearScreen,
	printHeader,nl,nl,nl,
	initial(GameState,Size),
	game_loop(GameState, 1, 'BotEasyVsBotHard', Time, Size).

% iniciar o jogo bot easy contra bot dumb
botEasyVsBotDumb:-
	boardSizeMenu(Size),
	botTimeMenu(Time),
	clearScreen,
	printHeader,nl,nl,nl,
	initial(GameState,Size),
	game_loop(GameState, 1, 'BotEasyVsBotDumb', Time, Size).

% iniciar o jogo bot hard contra bot hard
botHardVsBotHard:-
	boardSizeMenu(Size),
	botTimeMenu(Time),
	clearScreen,
	printHeader,nl,nl,nl,
	initial(GameState,Size),
	game_loop(GameState, 1, 'BotHardVsBotHard', Time, Size).

% iniciar o jogo bot hard contra bot dumb
botHardVsBotDumb:-
	boardSizeMenu(Size),
	botTimeMenu(Time),
	clearScreen,
	printHeader,nl,nl,nl,
	initial(GameState,Size),
	game_loop(GameState, 1, 'BotHardVsBotDumb', Time, Size).

% iniciar o jogo bot dumb contra bot dumb
botDumbVsBotDumb:-
	boardSizeMenu(Size),
	botTimeMenu(Time),
	clearScreen,
	printHeader,nl,nl,nl,
	initial(GameState,Size),
	game_loop(GameState, 1, 'BotDumbVsBotDumb', Time, Size).

% iniciar o jogo jogador contra jogador
playerVsPlayer:-
	boardSizeMenu(Size),
	clearScreen,
	printHeader,nl,nl,nl,
	initial(GameState,Size),
	game_loop(GameState, 1, 'PlayerVsPlayer', 0, Size).

% iniciar o jogo jogador contra bot easy
playerVsBotEasy:-
	boardSizeMenu(Size),
	botTimeMenu(Time),
	clearScreen,
	printHeader,nl,nl,nl,
	initial(GameState,Size),
	game_loop(GameState, 1, 'PlayerVsBotEasy', Time, Size).

% iniciar o jogo jogador contra bot hard
playerVsBotHard:-
	boardSizeMenu(Size),
	botTimeMenu(Time),
	clearScreen,
	printHeader,nl,nl,nl,
	initial(GameState,Size),
	game_loop(GameState, 1, 'PlayerVsBotHard', Time, Size).

% iniciar o jogo jogador contra bot dumb
playerVsBotDumb:-
	boardSizeMenu(Size),
	botTimeMenu(Time),
	clearScreen,
	printHeader,nl,nl,nl,
	initial(GameState,Size),
	game_loop(GameState, 1, 'PlayerVsBotDumb', Time, Size).

% menu do jogar novamente
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

% imprime o menu do jogar novamente
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

% imprime as instruções do jogo
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
		
