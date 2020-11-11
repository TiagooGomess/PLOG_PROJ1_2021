:- use_module(library(aggregate)).
:- use_module(library(lists)).

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

test:-
    L = [[1,2,0,2],[2,1,0,2],[3],[3],[3],[3],[3],[2]],
    checkIfEmpty(L, 0, 7).