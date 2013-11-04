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
    printBoard(B),
    P is 1,
    readPlayer(P).
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

readPlayer(P) :-
    write('PLAYER '),
    write(P),
    write(', Your turn! Choose move type (group/grow): '),
    read(M),
    playPlayer(P, M).

playPlayer(P, 'group').
    % MISSING
    % pergunta onde vai colocar a peça e tal
    % verifica se vale
    % se nao valer volta a ser chamada
    %checkGameEnded.

playPlayer(P, 'grow').
    % MISSING
    % pergunta onde vai colocar a peças recursivamente
    % verifica se vale
    % se nao valer volta a ser chamada
    %checkGameEnded.

% NOT WORKING
playPlayer(P, M) :-
    write('Invalid Move'),
    nl.

%checkGameEnded().
%
%endGame().

