:- use_module(library(aggregate)).
:- use_module(library(lists)).

occurrences_of(List, X, Count) :- aggregate_all(count, member(X, List), Count).

test:-
    L = [0,0,0,1,2,2,1,2,1],
    occurrences_of(L,2,Count),
    write(Count).