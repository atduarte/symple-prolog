% Create

createBoard(B, S) :-
	length( B, S ),
	createLists(B,S).

createLists([], S).
createLists([H|T], S) :-
	length( H, S ),
   	createPoint( H ),
   	createLists(T, S).
	
createPoint( [] ).
createPoint( [H|T] ) :-
	length( H, 2 ),
    fillPoint( H ),
    createPoint( T ).

fillPoint([]).
fillPoint( [P, G] ) :-
    P is 0,
    G is 0.

% Print Init

printBoard(B) :-
	N1 is 1,
	N2 is 1,
	printBoardTop(B, N1),
	printBoardLine(B),
	printBoardAux(B, N2).

% Print Line

printBoardLine(B) :- 
	write('   '),
	printBoardLineAux(B),
	nl.

printBoardLineAux([]).
printBoardLineAux([X | Y]) :-
	write('--- '),
	printBoardLineAux(Y).

% Top

printBoardTop(B, N) :- 
	write('   '),
	printBoardTopAux(B, N),
	nl.

printBoardTopAux([], N).
printBoardTopAux([X | Y], N) :-
	write(' '),
	write(N),
	write('  '),
	N1 is N+1,
	printBoardTopAux(Y, N1).

% Print Main

printBoardAux([], N).
printBoardAux([H | T], N) :-
	write(N),
	printList(H),
	nl,
	printBoardLine(H),
	N1 is N+1,
	printBoardAux(T, N1).

printList(L) :- 
	printListAux(L).

printListAux([]).
printListAux([H]) :-
	write(' | '),
	printPoint(H),
	write(' | ').
printListAux([H | T]) :-
	write(' | '),
	printPoint(H),
	printListAux(T).
printPoint([0 , J]) :-
   	write('0').
printPoint([P , J]) :-
	write(P).
