:- use_module(library(between)).



test:-
    findall([Row,Column], getRowAndColumn(Row,Column), Bag),
    write(Bag).
    