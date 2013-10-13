% Create

createBoard(L, S) :-
	length( L, S ),
	createLists(L,S).

createLists([], S).
createLists([H|T], S) :-
	length( H, S ),
   	fillList( H ),
   	createLists(T, S).
	
fillList( [] ).
fillList( [X|Xs] ) :-
    X is 0,
    fillList( Xs ).

% Print

printBoard(B) :-
	printBoardAux(B).

printBoardAux([]).
printBoardAux([H]) :- 
	printList(H).
printBoardAux([X | Y]) :-
	printList(X),
	write(' '),
	nl,
	printBoardAux(Y).

printList(L) :- 
	printListAux(L).

printListAux([]).
printListAux([H]) :- write(H).
printListAux([X | Y]) :-
   write( X ),
   write(' '),
   printListAux(Y).

% Play

playSymple :- 
	write('Choose board size: '),
	read(S),
	playSympleAux(S).

playSymple(S) :- 
	playSympleAux(S).

playSympleAux(S) :-
	createBoard(B, S),
	printBoard(B).

