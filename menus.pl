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
        Input = '2' -> helpMenu;
        Input = '3';

        nl, write('Invalid input!'), nl,
        start
    ).

gameMenu:-
	createBoard(Board, 6),
	write('Created Board'),nl,
	printBoard(Board),nl,
	write('Done!').