:- use_module(library(aggregate)).
:- use_module(library(lists)).

% se não tivessemos o get_char(_), não conseguíamos obter o próximo input
getChar(Input):-
	get_char(Input),nl,
	get_char(_).

% https://stackoverflow.com/questions/16908764/clearing-screen-in-swipl-prolog-in-windows#:~:text=Plain%20clear%20screen%3A,%5Ce%5B2J').
clearScreen:-write('\e[2J').

% espera que o user clique numa tecla para continuar
pressEnterToContinue:-
	write('Press <Enter> to continue.'), nl,
	get_char(_), !.

printHeader:-
    write('=================================='), nl,
	write('=       :::: Greener ::::        ='), nl,
	write('=================================='), nl.
	

% Faz a diferença de L1 e L2 e mete em L; L1 é uma lista normal, L2 uma lista de listas,
% em que cada lista interior só tem um elemento (ex.: L2 = [[0],[1],[2]),
% L é uma lista normal
remove_elements(L1, L2, L):-
	append(A, B, L1),
	flatten2(L2, L3), % se L2 = [[0],[1],[2]], então L3 = [0,1,2]
    append(C, L3, A),
    append(C, B, L).
	
% Converte uma lista de listas numa única lista. 
%(https://stackoverflow.com/questions/9059572/flatten-a-list-in-prolog)
flatten2([], []) :- !.
flatten2([L|Ls], FlatL) :-
    !,
    flatten2(L, NewL),
    flatten2(Ls, NewLs),
    append(NewL, NewLs, FlatL).
flatten2(L, [L]).

translate(0, ' W '). % white pyramid
translate(1, ' B '). % black pyramid
translate(2, ' G '). % green pyramid
translate(3, ' - '). % no piece

% counts the Count occorences of X in List
occurrences_of(List, X, Count):-
    aggregate_all(count, member(X, List), Count).