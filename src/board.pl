:- use_module(library(random)).
:- use_module(library(lists)).

:-ensure_loaded('utils.pl').

% cria um tabuleiro, no formato lista de listas de listas, em que as listas mais interiores
% são formadas por apenas um elemento, já que no início as stacks têm todas uma altura de 1
createBoard([[]], 0, _).
createBoard(Board, Size, Pieces):-
	Size > 0,
	Size1 is Size - 1,
	createLine(6, Pieces, Line),
	remove_elements(Pieces, Line, RemainingPieces),
	Board = [Line | T],
	createBoard(T, Size1, RemainingPieces).
	
% copia N elementos de L1 (lista com todas as peças restantes) para [[X]|T] de forma aleatória
% [[X]|T] é uma lista de listas; esta função cria uma linha em que cada célula da linha possui
% uma lista com um elemento
createLine(0, _, []).
createLine(N, L1, [[X]|T]):-
	N > 0,
	N1 is N - 1,
	random_member(X, L1),
	select(X, L1, Remaining),
	createLine(N1, Remaining, T).
	
% imprime o tabuleiro, incluindo os números das linhas e colunas
printBoard(_,[]):-
	write('  |-----|-----|-----|-----|-----|-----|'),nl,
    write('     A     B     C     D     E     F ').
printBoard([H|T],[Row|RowT]):-
	write('  |-----|-----|-----|-----|-----|-----|'),nl,
	write(Row),
	write(' '),
	printLine(H),
	write('|'),
	nl,
	printBoard(T,RowT).
	
% imprime a Head de cada lista da linha e o número de pirâmides verdes, em cada célula (caso existam)
printLine([]).
printLine([[H|_T]|T]):-
	write('|'),
	occurrences_of([H|_T],2,NumGreen),
	translate(H, Piece),
	writeInCell(Piece, NumGreen),
	printLine(T).

% Escreve o número de pirâmides verdes de uma stack, caso existam
writeNumGreen(0):-
	write(' ').
writeNumGreen(Num):-
	write(Num).

% escreve a informação na célula do tabuleiro
writeInCell(Piece, 0):-
	write(' '),
	write(Piece),
	write(' ').
writeInCell(Piece, NumGreen):-
	write(Piece),
	write(NumGreen),
	write(' ').

% dá-nos o número da peça que está no topo da stack, dada uma linha e uma coluna
getPieceByRowAndColumn(Board, Row, Column, Piece):-
    nth0(Row,Board,RowList),
    nth0(Column,RowList,[Piece|_]).

% dá-nos a stack de peças que está na posição (Column, Row)
getStackByRowAndColumn(Board, Row, Column, Stack):-
    nth0(Row,Board,RowList),
    nth0(Column,RowList,Stack).

% pergunta ao jogador qual a posição da peça que quer mover
askForPiecePos(Row, Column, Question):-
	repeat,
	nl,nl,
	write(Question),nl,nl,
	write('Column: '),
	getChar(Input1),
	translate_column(Input1, Column),
	write('Row: '),
	getInt(Input2),
	Input2 =< 6,
	Input2 >= 1,
	translate_row(Input2, Row).

% pergunta ao jogador a peça que quer mover e para que posição
askMove(Board, RowStart, ColumnStart, RowEnd, ColumnEnd, Piece):-
	askForPiecePos(RowStart, ColumnStart, 'Which stack do you want to move to?'),nl,nl,
	askForPiecePos(RowEnd, ColumnEnd, 'For which position do you want to move it?'),nl,nl,
	getPieceByRowAndColumn(Board, RowStart, ColumnStart, Piece).

% move a stack da posição (ColumnStart,RowStart) para (ColumnEnd,RowEnd)
% Piece holds the piece on the top of the stack that was moved
makeMove(Board, NewBoad, RowStart, ColumnStart, RowEnd, ColumnEnd, Piece):-
	getStackByRowAndColumn(Board, RowStart, ColumnStart, StackStart), % todo limpar o conteudo desta célula
	getStackByRowAndColumn(Board, RowEnd, ColumnEnd, StackEnd),

	append_stack(Board, RowEnd, ColumnEnd, StackStart, NewBoad).


% adiciona Stack no topo da stack que está na posição (Row, Column)
% e muda o estado do tabuleiro de acordo com esse movimento;
% começa por procurar a linha desejada, e quando a encontrar, chama o predicado 
% search_column/4 de forma a adicionar Stack ao conteúdo da célula desejada
append_stack([BoardRow|RemainingBoardRows], 0, Column, Stack, [NewBoardRow|RemainingBoardRows]):-
	search_column(BoardRow, Column, Stack, NewBoardRow).

append_stack([BoardRow|RemainingBoardRows], Row, Column, Stack, [BoardRow|RemainingNewBoardRows]):-
	Row > 0,
	Row1 is Row-1,
	append_stack(RemainingBoardRows, Row1, Column, Stack, RemainingNewBoardRows).

% predicado usado em append_stack/5
% procura por uma coluna numa linha do tabuleiro, de forma a adionar Stack ao conteúdo da célula desejada
search_column([BoardColumn|RemainingBoardColumns], 0, Stack, [NewBoardColumn|RemainingBoardColumns]):-
	append(Stack, BoardColumn, NewBoardColumn).

search_column([BoardColumn|RemainingBoardColumns], Column, Stack, [BoardColumn|RemainingNewBoardColumns]):-
	Column > 0,
	Column1 is Column-1,
	search_column(RemainingBoardColumns, Column1, Stack, RemainingNewBoardColumns).