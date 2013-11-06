%%%%%%%%%%%%%%%%%%%%%%%%%%%  Init Play Symple  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
    write('Creating Board'), nl,
    createBoard(B, S),
    printBoard(B),
    P is 1,
    !,
    readPlayer(P, B).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%   Read Player  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

readPlayer(P, B) :-
    %countGroups(B, GP1, GP2),
    %write('GP1 '), write(GP1), write(' / GP2 '), write(GP2), nl,
    %countPieces(B, P1, P2, F),
    %write('P1 '), write(P1), write(' / P2 '), write(P2), write(' / Free '), write(F), nl,
    checkGameEnd(B),
    write('Game Ended.'), nl.
readPlayer(P, B) :-
    write('PLAYER '),
    write(P),
    write(', Your turn! Choose move type (create/grow): '),
    read(M),
    playPlayer(P, B, B1, M),
    changePlayer(P, P1),
    printBoard(B1),
    readPlayer(P1, B1).
readPlayer(P, B) :- readPlayer(P, B).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%   Moves  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

playPlayer(P, B, B1, M) :-
    M \= 'group',
    M \= 'create',
    write('Invalid Move'), nl,
    false.
    
% MISSING
% pergunta onde vai colocar a peças recursivamente
% verifica se vale
% se nao valer volta a ser chamada
%checkGameEnded,
playPlayer(P, B, B1, 'grow') :-
    append(B, [], B1).

playPlayer(P, B, B1, 'create') :-
    write('Choose column to place group: '),
    read(C),
    write('Choose line to place group'),
    read(L),
    checkPlace(B, C, L),
    % TODO Tem de verificar adjacencias
    getNextGroup(B, P, G),
    placePiece(P, G, B, B1, C, L).
playPlayer(P, B, B1, 'create') :-
    write('Invalid position.'), nl,
    false.



%checkGameEnded().
%
%endGame().

