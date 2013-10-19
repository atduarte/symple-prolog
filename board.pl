% Create

createBoard(B, S) :-
	length( B, S ),
	createLists(B,S).

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
	N1 is 1,
	N2 is 1,
	write(' '),
	printBoardTop(B, N1),
	nl,
	printBoardAux(B, N2).

printBoardTop([], N).
printBoardTop([X | Y], N) :-
	write(' '),
	write(N),
	N1 is N+1,
	%N is N1+1,
	printBoardTop(Y, N1).


printBoardAux([], N).
printBoardAux([H], N) :- 
	write(N),
	printList(H).
printBoardAux([X | Y], N) :-
	write(N),
	printList(X),
	nl,
	N1 is N+1,
	printBoardAux(Y, N1).


printList(L) :- 
	printListAux(L).

printListAux([]).
printListAux([H]) :- write(' '), write(H).
printListAux([X | Y]) :-
   write(' '),
   write( X ),
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

