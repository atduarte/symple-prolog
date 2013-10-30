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
	printBoard(B),
	readPlayer1.

readPlayer1 :-
	write('PLAYER 1, Your turn! Choose move type (group/grow): '),
	read(P),
	playPlayer1(P).

playPlayer1('group') :-
	groupPlayer1,
	checkGameEnded.

playPlayer1('grow') :-
	growPlayer1,
	checkGameEnded.

groupPlayer1 :-
	% pergunta onde vai colocar a peça e tal
	% verifica se vale
	% se nao valer volta a ser chamada
	checkGameEnded,
	readPlayer2.

groupPlayer1 :-
	% pergunta onde vai colocar a peças recursivamente
	% verifica se vale
	% se nao valer volta a ser chamada
	checkGameEnded,
	readPlayer2.

readPlayer2 :-
	write('PLAYER 2, Your turn! Choose move type (group/grow): '),
	read(P),
	playPlayer2(P).

playPlayer2('group') :-
	groupPlayer2,
	checkGameEnded.

playPlayer2('grow') :-
	growPlayer2,
	checkGameEnded.

groupPlayer2 :-
	% pergunta onde vai colocar a peça e tal
	% verifica se vale
	% se nao valer volta a ser chamada
	checkGameEnded,
	readPlayer1.

groupPlayer2 :-
	% pergunta onde vai colocar a peças recursivamente
	% verifica se vale
	% se nao valer volta a ser chamada
	checkGameEnded,
	readPlayer1.





