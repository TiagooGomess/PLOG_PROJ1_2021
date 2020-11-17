% verifica se as células da lista List estão vazias ([3]), com excepção da célula de index Pos
checkIfEmptyUnless(List,Pos):-
    length(List, Len),
    PosInverted is Len - Pos,
    checkIfEmptyUnless(List,PosInverted,Len).
checkIfEmptyUnless([],_,_).
checkIfEmptyUnless([[H|_]|T], Pos, Len):-
    Len > 0,
    (
        Len = Pos -> H \= 3;
        H = 3
    ),
    Len1 is Len - 1,
    checkIfEmptyUnless(T, Pos, Len1).



    