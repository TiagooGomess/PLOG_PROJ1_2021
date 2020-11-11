:-ensure_loaded('utils.pl').
:-ensure_loaded('board.pl').
:-ensure_loaded('game.pl').

play:-
    printMainMenu,
    getMainMenuOption.

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

getMainMenuOption:-
    getChar(Input),(
        Input = '1' -> gameMenu;
        Input = '2' -> helpMenu, play; % entra em helpMenu e volta para play.
        Input = '3';

        nl, write('Invalid input!'), nl,
		pressEnterToContinue,nl,
        start
    ).

gameMenu:-
	clearScreen,
	printHeader,nl,nl,nl,
	initial(GameState),
	game_loop(GameState, 1).
	/*
	nl,
	write(GameState),
	nl,
	display_game(GameState, 1), % Black player (1) starts

	
	askMove(GameState, RowStart, ColumnStart, RowEnd, ColumnEnd, Piece),
	makeMove(GameState, NewBoard, RowStart, ColumnStart, RowEnd, ColumnEnd, Piece),
	nl,
	write(NewBoard),
	nl,
	display_game(NewBoard, 1).*/

	/*
	askForPiecePos(Row,Column),nl,
	getPieceByRowAndColumn(GameState, Row, Column, PieceN),nl,nl,
	translate(PieceN, Piece),
	write('The piece you chose was'),
	write(Piece),nl,nl.
	*/
	%--------------------------------------------------------------------------------------------------------

	% as linhas de baixo servem para testar o caso em que há linhas vazias e stacks com mais do que 1 de altura
	% comentar a initial e a display_game de cima para testar
	
	%% intermediate state:
	%L = [ [[1,1,0],[3],[3],[2],[0],[2]],[[0],[0],[2],[1],[2],[0]],[[1],[1],[2],[1],[2],[2]],[[1,2,2,2],[3],[3],[3],[0],[2]],[[2],[0],[2],[1],[2],[2]],[[1],[0],[2],[2],[0],[2]] ],
	%display_game(L,0).
	
	%% final state:
	%L = [ [[1,1,0,2,0,2],[3],[3],[3],[3],[3]], [[3],[3],[3],[1,2,0,0,2,0],[3],[3]], [[3],[1,1,2,1,2,2],[3],[3],[3],[3]], [[3],[3], [1,2,2,2,2,0,2],[3],[3],[3]], [[3],[3],[3],[3],[3],[1,2,2,2,0]], [[3],[3],[3],[3],[0,2,2,2,0,1],[3]] ],
	%printBoard(L, [6,5,4,3,2,1]),nl,nl,
	%write('The Black player won!'),nl,nl,nl.

	%--------------------------------------------------------------------------------------------------------

	
helpMenu:-
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
		
