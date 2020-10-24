getChar(Input):-
	get_char(Input),
	write(Input),
	nl,
	get_char(_).
	
% Faz a diferença de L1 e L2 e mete em L
remove_elements(L1, L2, L) :-
    append(A, B, L1),
    append(C, L2, A),
    append(C, B, L).