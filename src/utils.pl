:- use_module(library(aggregate)).
:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(system), [now/1]).

init_random_state:-
    now(X),
    setrand(X).

% se não tivessemos o get_char(_), não conseguíamos obter o próximo input
getChar(Input):-
	get_char(Input),nl,
	get_char(_).

% converte de char para inteiro
getInt(Input):-
    get_code(Aux),
    get_code(_),
	Input is Aux - 48.

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
	
% remove uma ocorrência de um elemento Elem de uma lista
delete_one(_, [], []).
delete_one(Elem, [Elem|T], T).
delete_one(Elem, [H|T], [H|Result]) :-
  delete_one(Elem, T, Result).

% remove os elementos que estão na segunda lista da primeira lista, de acordo
% com o número de ocorrências dos mesmos
remove_elements(Pieces, [], Pieces).
remove_elements(Pieces, [HLine|TLine], RemainingPieces):-
    delete_one(HLine,Pieces,RemainingPieces0),
    remove_elements(RemainingPieces0,TLine,RemainingPieces).
	
% Converte uma lista de listas numa única lista. 
%(https://stackoverflow.com/questions/9059572/flatten-a-list-in-prolog)
flatten2([], []) :- !.
flatten2([L|Ls], FlatL) :-
    !,
    flatten2(L, NewL),
    flatten2(Ls, NewLs),
    append(NewL, NewLs, FlatL).
flatten2(L, [L]).

% lista vazia
empty([]).

% Mapeamento da representação interna das pirâmides para a visualização do tabuleiro
translate(0, ' W '). % white pyramid
translate(1, ' B '). % black pyramid
translate(2, ' G '). % green pyramid
translate(3, ' - '). % no piece

% counts the Count occorences of X in List
occurrences_of(List, X, Count):-
    aggregate_all(count, member(X, List), Count).

% número das linhas a ser usado no display do tabuleiro
row_numbers([6,5,4,3,2,1]).

% translates the letters of the columns that are visible to the players to the real indexes
translate_column('A', 0).
translate_column('B', 1).
translate_column('C', 2).
translate_column('D', 3).
translate_column('E', 4).
translate_column('F', 5).
translate_column('a', 0).
translate_column('b', 1).
translate_column('c', 2).
translate_column('d', 3).
translate_column('e', 4).
translate_column('f', 5).

% translates the numbers of the rows that are visible to the players to the real indexes
translate_row(6, 0).
translate_row(5, 1).
translate_row(4, 2).
translate_row(3, 3).
translate_row(2, 4).
translate_row(1, 5).

% verifica se o movimento é feito ortogonalmente
checkOrthogonality(RowFrom, ColumnFrom, RowTo, ColumnTo):-
    RowFrom = RowTo;
    ColumnFrom = ColumnTo.
    
% verifica se as células da lista List estão vazias ([3]) entre o index PosFrom e PosTo (não inclusive)
checkIfEmpty(List, PosFrom, PosTo):-
    PosFrom1 is PosFrom + 1, 
    Count is PosTo - PosFrom,
    sub_list(List, PosFrom1, Count, SubList), % sub-lista que queremos verificar se está vazia (só com [3])
    Len is Count - 1,
    checkIfEmpty(SubList, Len).
% se chegarmos a este ponto de execução, em que a lista foi toda processada, estão a lista só tem elementos [3]
checkIfEmpty(_, 0).
% percorre a lista e só continua a execução se o elemento da lista é [3]
checkIfEmpty([[H|_]|T], Len):-
    Len > 0,
    Len1 is Len - 1,
    H = 3,
    checkIfEmpty(T, Len1).

% https://stackoverflow.com/questions/20765479/create-a-sublist-from-a-list-given-an-index-and-a-number-of-elements-prolog
% encontra a sublista de L, desde o index M até M + N, ficando a sub-lista em S.
sub_list(L, M, N, S) :-
    findall(E, (nth0(I, L, E), I >= M, I =< N), S).