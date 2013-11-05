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
    readPlayer(P, B).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%   Read Player  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

readPlayer(P, B) :-
    checkGameEnd(B),
    write('Game Ended.'), nl.
readPlayer(P, B) :-
    write('PLAYER '),
    write(P),
    write(', Your turn! Choose move type (group/grow): '),
    read(M),
    playPlayer(P, B, B1, M),
    changePlayer(P, P1),
    printBoard(B1),
    readPlayer(P1, B1).
readPlayer(P, B) :- readPlayer(P, B).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%   Moves  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

playPlayer(P, B, B1, 'grow').
    % MISSING
    % pergunta onde vai colocar a peças recursivamente
    % verifica se vale
    % se nao valer volta a ser chamada
    %checkGameEnded.

playPlayer(P, B, B1, 'group') :-
    % MISSING
    % pergunta onde vai colocar a peça e tal
    % verifica se vale
    % se nao valer volta a ser chamada
    %checkGameEnded.
    write('Choose column to place group: '),
    read(C),
    write('Choose line to place group'),
    read(L),
    placePiece(P, B, B1, C, L). % Tem de verificar a possibilidade. Retorna false se nao for possivel. Coloca se for
playPlayer(P, B, B1, 'group') :- playPlayer(P, B, B1, 'group').

playPlayer(P, B, B1, M) :-
    M \= 'group',
    M \= 'grow',
    write('Invalid Move'), nl,
    false.

%checkGameEnded().
%
%endGame().

