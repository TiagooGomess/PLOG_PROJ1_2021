:- use_module(library(random)).
:- use_module(library(lists)).

:-ensure_loaded('utils.pl').

% cria um tabuleiro, no formato lista de listas de listas, em que as listas mais interiores
% são formadas por apenas um elemento, já que no início as stacks têm todas uma altura de 1
createBoard(Board, Size, Pieces):-
	createBoard(Board, [], Size, Pieces).
createBoard(Board, Board, 0, []).
createBoard(B, Board, Size, Pieces):-
	Size > 0,
	Size1 is Size - 1,
	createLine(6, Pieces, Line),
	flatten2(Line,Line1), % se Line = [[0],[1],[2]], então Line1 = [0,1,2]
	remove_elements(Pieces, Line1, RemainingPieces),
	createBoard(B, [Line|Board], Size1, RemainingPieces).
	
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

% escreve a informação na célula do tabuleiro
writeInCell(Piece, 0):-
	write(' '),
	write(Piece),
	write(' ').
writeInCell(Piece, NumGreen):-
	NumGreen < 10, % caso em que o número de peças verdes só ocupa 1 caracter
	write(Piece),
	write(NumGreen),
	write(' '),!.
writeInCell(Piece, NumGreen):-
	NumGreen > 9, % caso em que o número de peças verdes ocupa 2 caracteres
	write(Piece),
	write(NumGreen).

% dá-nos o número da peça que está no topo da stack, dada uma linha e uma coluna
getPieceByRowAndColumn(Board, Row, Column, Piece):-
    nth0(Row,Board,RowList),
    nth0(Column,RowList,[Piece|_]).

% dá-nos a stack de peças que está na posição (Column, Row)
getStackByRowAndColumn(Board, Row, Column, Stack):-
    nth0(Row,Board,RowList),
    nth0(Column,RowList,Stack).

% pergunta ao jogador uma posição do tabuleiro
askForPosition(Row, Column, Question):-
	repeat,
	(
		(	
			nl,nl,
			write(Question),nl,nl,
			write('Column: '),
			getChar(Input1),
			translate_column(Input1, Column),
			write('Row: '),
			getInt(Input2),
			Input2 =< 6,
			Input2 >= 1,
			translate_row(Input2, Row),!
		);
		nl,nl,write('This position is not a valid one!\nThe number of the rows are between 1 and 6 and the columns are between A and F!'),nl,nl,fail
	).
	
% pergunta ao jogador qual a posição da peça que quer mover e verifica se é válida
askForPiecePosFrom(Board, Player, Row, Column):-
	repeat,
	askForPosition(Row, Column, 'Which stack do you want to move?'),
	getPieceByRowAndColumn(Board, Row, Column, Piece),
	(
		(
			Piece = Player, % verifica se a stack que o jogador quer mover lhe pertence
			\+ checkIfStackCannotCapture(Board, Row, Column),!
		);
		nl,nl,write('You connot move that stack!\nPlease choose another one!'),nl,nl,fail
	).
	
% pergunta ao jogador qual a posição para onde quer mover a peça e verifica se é válida
askForPiecePosTo(Board, RowFrom, ColumnFrom, RowTo, ColumnTo):-
	repeat,
	askForPosition(RowTo, ColumnTo, 'For which position do you want to move it?'),
	(
		(
			checkOrthogonality(RowFrom, ColumnFrom, RowTo, ColumnTo), % verifica se o movimento é feito ortogonalmente
			checkStacksBetween(Board, RowFrom, ColumnFrom, RowTo, ColumnTo), % verifica se existem stacks entre a posição inicial e final
			\+ checkEmptyCell(Board, RowTo, ColumnTo),! % verifica se a posição final tem alguma peça para ser capturada
		);
		nl,nl,write('This movement is not a valid one!\nPlease choose another final position!'),nl,nl,fail
	).
	
% verifica se a posição (Row, Column) está vazia
checkEmptyCell(Board, Row, Column):-
	getPieceByRowAndColumn(Board, Row, Column, Piece),
	Piece = 3.

% verifica se existem stacks entre a posição inicial e final;
% neste ponto de execução, há garantia que a posição inicial e final são ortogonais.
% caso em que as stacks estão na mesma linha
checkStacksBetween(Board, Row, ColumnFrom, Row, ColumnTo):-
	ColumnFrom \= ColumnTo, % a stack não pode ficar na mesma posição
	nth0(Row,Board,RowList), % RowList contém a linha onde as stacks se encontram
	(
		ColumnFrom < ColumnTo
		-> checkIfEmptyBetween(RowList, ColumnFrom, ColumnTo);
		checkIfEmptyBetween(RowList, ColumnTo, ColumnFrom)
	).
% caso em que as stacks estão na mesma coluna
checkStacksBetween(Board, RowFrom, Column, RowTo, Column):-
	RowFrom \= RowTo, % a stack não pode ficar na mesma posição
	getColumnN(Board, Column, ColumnList), % ColumnList contém a coluna onde as stacks se encontram
	(
		RowFrom < RowTo
		-> checkIfEmptyBetween(ColumnList, RowFrom, RowTo);
		checkIfEmptyBetween(ColumnList, RowTo, RowFrom)
	).

% dá-nos a ColumnList na posição de index Column de Board
getColumnN([],_,[]).
getColumnN([BoardHeader|BoardRemaining], Column, [ColumnListHeader|ColumnListRemaining]):-
	nth0(Column, BoardHeader, ColumnListHeader),
	getColumnN(BoardRemaining, Column, ColumnListRemaining).

% pergunta ao jogador que peça quer mover e para que posição;
% pergunta sempre até ao jogador inserir posições iniciais e finais válidas
askMove(Board, Player, RowStart, ColumnStart, RowEnd, ColumnEnd):-
	nl,write('========================================'),nl,
	askForPiecePosFrom(Board, Player, RowStart, ColumnStart),nl,
	askForPiecePosTo(Board, RowStart, ColumnStart, RowEnd, ColumnEnd),
	nl,nl,write('========================================'),nl,nl.

% move a stack da posição (ColumnStart,RowStart) para (ColumnEnd,RowEnd)
makeMove(Board, NewBoad, RowStart, ColumnStart, RowEnd, ColumnEnd):-
	getStackByRowAndColumn(Board, RowStart, ColumnStart, StackStart),
	append_stack(Board, RowEnd, ColumnEnd, StackStart, NewBoad0),
	clear_cell(NewBoad0, RowStart, ColumnStart, NewBoad).

% adiciona Stack no topo da stack que está na posição (Row, Column)
% e muda o estado do tabuleiro de acordo com esse movimento;
% começa por procurar a linha desejada, e quando a encontrar, chama o predicado 
% search_column/4 de forma a adicionar Stack ao conteúdo da célula desejada
append_stack([BoardRow|RemainingBoardRows], 0, Column, Stack, [NewBoardRow|RemainingBoardRows]):-
	search_column(BoardRow, Column, Stack, NewBoardRow).
append_stack([BoardRow|RemainingBoardRows], Row, Column, Stack, [BoardRow|RemainingNewBoardRows]):-
	Row > 0,
	Row1 is Row - 1,
	append_stack(RemainingBoardRows, Row1, Column, Stack, RemainingNewBoardRows).

% predicado usado em append_stack/5
% procura por uma coluna numa linha do tabuleiro, de forma a adionar Stack ao conteúdo da célula desejada
search_column([BoardColumn|RemainingBoardColumns], 0, Stack, [NewBoardColumn|RemainingBoardColumns]):-
	append(Stack, BoardColumn, NewBoardColumn).
search_column([BoardColumn|RemainingBoardColumns], Column, Stack, [BoardColumn|RemainingNewBoardColumns]):-
	Column > 0,
	Column1 is Column - 1,
	search_column(RemainingBoardColumns, Column1, Stack, RemainingNewBoardColumns).

% este predicado é similar ao append_stack, mas em vez de adicionar uma stack ao conteúdo de uma célula,
% substitui o conteúdo de uma célula pela nossa representação de célula vazia ([3]).
clear_cell([BoardRow|RemainingBoardRows], 0, Column, [NewBoardRow|RemainingBoardRows]):-
	search_column_to_clear(BoardRow, Column, NewBoardRow).
clear_cell([BoardRow|RemainingBoardRows], Row, Column, [BoardRow|RemainingNewBoardRows]):-
	Row > 0,
	Row1 is Row - 1,
	clear_cell(RemainingBoardRows, Row1, Column, RemainingNewBoardRows).

search_column_to_clear([_|RemainingBoardColumns], 0, [[3]|RemainingBoardColumns]).
search_column_to_clear([BoardColumn|RemainingBoardColumns], Column, [BoardColumn|RemainingNewBoardColumns]):-
	Column > 0,
	Column1 is Column - 1,
	search_column_to_clear(RemainingBoardColumns, Column1, RemainingNewBoardColumns).

% conta o número de stacks pertencentes a um dado jogador.
countPlayerStacks(Board, Player, NumStacks):-
	countPlayerStacks(Board, Player, NumStacks, 0, 6).
countPlayerStacks(_, _, NumStacks, NumStacks, 0).
countPlayerStacks(Board, Player, N, NumStacks, Row):-
	Row > 0,
	Row1 is Row - 1,
	nth0(Row1, Board, RowList),
	countRowStacks(Player, RowList, Counter),
	NumStacks1 is NumStacks + Counter,
	countPlayerStacks(Board, Player, N, NumStacks1, Row1).

% conta o número de stacks pertencentes a um dado jogador, numa linha.
countRowStacks(Player, RowList, Counter):-
	countRowStacks(Player, RowList, Counter, 0).
countRowStacks(_, [], Counter, Counter).
countRowStacks(Player, [[H|_]|T], C, Counter):-
	(
		H = Player -> Counter1 is Counter + 1;
		Counter1 is Counter
	),
	countRowStacks(Player, T, C, Counter1).

% sucede se o jogador não consegue fazer nenhum movimento
% TODO: mudar para suceder quando os dois jogadores passam a jogada sucessivamente
checkEnd(Board, Player):- 
	\+ checkIfPlayerCanMakeMove(Board,Player).

% verifica se uma stack não pode capturar outras, ou seja, se não há nenhuma stack
% na mesma linha ou na mesma coluna
checkIfStackCannotCapture(Board, Row, Column):-
	nth0(Row,Board,RowList),
	checkIfEmptyUnless(RowList,Column),
	getColumnN(Board,Column,ColumnList),
	checkIfEmptyUnless(ColumnList,Row).

% verifica se o Player consegue fazer algum movimento, ou seja, 
% se tem pelo menos uma stack que tenha outra stack na mesma linha ou coluna
checkIfPlayerCanMakeMove(Board,Player):-
	checkIfPlayerCanMakeMove(Board,Board,Player,6).
checkIfPlayerCanMakeMove([],_,_,0):-fail.
checkIfPlayerCanMakeMove([Line|T],Board,Player,Row):-
	Row > 0,
	Row1 is Row - 1,
	(
		RealRow is 6 - Row,
		checkIfPlayerCanMakeMoveRow(Board,Line,Player,RealRow,6) -> !;
		checkIfPlayerCanMakeMove(T,Board,Player,Row1)
	).
	
% verifica se o Player consegue fazer algum movimento,
% para as stacks que tem numa certa linha
checkIfPlayerCanMakeMoveRow(_,[],_,_,0):-fail.
checkIfPlayerCanMakeMoveRow(Board,[[H|X]|T],Player,Row,Column):-
	Column > 0,
	Column1 is Column - 1,
	(
		H = Player -> (
			RealColumn is 6 - Column,
			\+ checkIfStackCannotCapture(Board, Row, RealColumn) -> !;
			fail
		);
		checkIfPlayerCanMakeMoveRow(Board,T,Player,Row,Column1)
	).

