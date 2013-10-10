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
	write('['), 
	printBoardAux(B),
	write(']').

printBoardAux([]).
printBoardAux([H]) :- 
	printList(H).
printBoardAux([X | Y]) :-
	printList(X),
	write(','),
	nl,
	write(' '),
	printBoardAux(Y).

printList(L) :-
	write('['), 
	printListAux(L),
	write(']').

printListAux([]).
printListAux([H]):- write(H).
printListAux([X | Y]) :-
   write( X ),
   write(' ,'),
   printListAux(Y).

% Play

play :- 
	write('Choose board size: '),
	read(S),
	createBoard(B, S),
	printBoard(B).
