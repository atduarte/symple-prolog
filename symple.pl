%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Includes %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:-  consult(board),
    playSymple.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   Play   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

playSymple :- 
    write('Choose board size: '),
    read(S),
    playSympleAux(S).

playSymple(S) :- 
    playSympleAux(S).

% Verifies if SIZE < 10
playSympleAux(S) :- 
    S > 9,
    write('Choose a table size smaller than 10'),
    nl,
    playSymple.

% Verifies if SIZE is an ODD number
playSympleAux(S) :- 
    R is mod(S,2),
    R = 0,
    write('Choose a odd table size'),
    nl,
    playSymple.

playSympleAux(S) :-
    S < 10,
    R is mod(S,2),
    R = 1,
    createBoard(B, S),
    printBoard(B),
    P is 1,
    readPlayer(P, B).

readPlayer(P, B) :-
    P = 1,
    write('PLAYER '),
    write(P),
    write(', Your turn! Choose move type (group/grow): '),
    read(M),
    playPlayer(P, B, M),
    printBoard(B),
    P is 2,
    readPlayer(P, B).

readPlayer(P, B) :-
    P = 2,
    write('PLAYER '),
    write(P),
    write(', Your turn! Choose move type (group/grow): '),
    read(M),
    playPlayer(P, B, M),
    printBoard(B),
    P is 1,
    readPlayer(P, B).

playPlayer(P, B, 'grow').
    % MISSING
    % pergunta onde vai colocar a peças recursivamente
    % verifica se vale
    % se nao valer volta a ser chamada
    %checkGameEnded.

playPlayer(P, B, 'group') :-
    % MISSING
    % pergunta onde vai colocar a peça e tal
    % verifica se vale
    % se nao valer volta a ser chamada
    %checkGameEnded.
    write('Choose column to place group: '),
    read(C),
    write('Choose line to place group'),
    read(L),
    placePiece(P, B, C, L, B1).

placePiece(P, [H|T], C, L, [H1|T1]) :-
    NL is 1,
    NC is 1,
    placePieceAux(P, H, C, L, H1, NC, NL),
    NL1 is NL + 1,
    placePiece(P, T, C, L, T1).


placePieceAux(P, [], C, L, [H1|T1], NC, NL).

placePieceAux(P, [H|T], C, L, [H1|T1], NC, NL) :-
    NC \= C,
    append(H, [], H1),
    NC1 is NC + 1,
    placePieceAux(P, T, C, L, T1, NC1, NL).

placePieceAux(P, [H|T], C, L, [H1|T1], NC, NL) :-
    NC = C,
    NL \= L,
    append(H, [], H1),
    NC1 is NC + 1,
    placePieceAux(P, T, C, L, T1, NC1, NL).

placePieceAux(P, [H|T], C, L, [H1|T1], NC, NL) :-
    NC = C,
    NL = L,
    append([2,1], [], H1),
    NC1 is NC + 1,
    placePieceAux(P, T, C, L, T1, NC1, NL).








% NOT WORKING
% playPlayer(P, M) :-
%    write('Invalid Move'),
%    nl.

%checkGameEnded().
%
%endGame().

