% Includes

:-  consult(board),
    playSymple.

%
% Play
%

playSymple :- 
    write('Choose board size: '),
    read(S),
    playSympleAux(S).

playSymple(S) :- 
    playSympleAux(S).

playSympleAux(S) :-
    S < 10,
    R is mod(S,2),
    R = 1,
    createBoard(B, S),
    printBoard(B).
% Verifies if SIZE < 10
playSympleAux(S) :- 
    S > 9,
    write('Choose a table size smaller than 10'),
    nl,
    playSymple.
% Verifies if SIZE is ODD
playSympleAux(S) :- 
    R is mod(S,2),
    R = 0,
    write('Choose a odd table size'),
    nl,
    playSymple.
