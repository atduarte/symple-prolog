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
    write(', Your turn! Choose move type (create/grow)'),
    read(M),
    playPlayer(P, B, B1, M),
    changePlayer(P, P1),
    printBoard(B1),
    readPlayer(P1, B1).
readPlayer(P, B) :- readPlayer(P, B).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%   Moves  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% INVALID MOVE
playPlayer(P, B, B1, M) :-
    M \= 'grow',
    M \= 'create', 
    !,
    write('Invalid Move'), nl,
    false.
    
% GROW
playPlayer(P, B, B1, 'grow') :-
    getPlayerGroups(B, P, GP),
    checkGrowGroups(GP),
    grow(P, B, B1, GP, []).

% CREATE
playPlayer(P, B, B1, 'create') :-
    askPosition(C, L),
    checkPlace(B, C, L),
    checkNotAdjacent(B, C, L, P),
    getNextGroup(B, P, G),
    placePiece(P, G, B, B1, C, L).
playPlayer(P, B, B1, 'create') :-
    write('Invalid position.'), nl,
    false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%   Grow  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

checkGrowGroups(GP) :-
    length(GP, X),
    X = 0, !,
    write('Invalid: You dont have groups to grow'), nl,
    false.
checkGrowGroups(GP).

% Já expandiu todos
grow(P, B, B, GP, EGP) :-
    length(GP, S1),
    length(EGP, S1).
grow(P, B, B1, GP, EGP) :-
    askPosition(C, L),
    getNextGroup(B, P, G),

    % Verificar se posição está livre
    checkPlace(B, C, L),

    % Procurar grupos adjacentes
    getAdjancentGroups(B, P, C, L, AGP),
    % Verificar que existem grupos adjacentes
    length(AGP, AGPS),
    %write('DEBUG AGPS:'), write(AGPS), write(AGP), nl,
    AGPS > 0,

    % Verificar que grupos adjacentes não foram já expandidos
    %write('DEGUG: '), write(EGP), write(AGP), nl,
    checkNotExpanded(EGP, AGP),

    % Mudar todos os pontos dos grupos adjacentes para um novo grupo
    % E mudar lista de grupos
    changeGroups(B, BN, P, GP, GP1, AGP, G),

    % Colocar peça
    placePiece(P, G, BN, BNN, C, L),

    % Criar nova lista de grupos expandidos
    append(EGP, [G], EGP1), 
    getPlayerGroups(B, P, GP2),
    write('DEGUG GROUPS: '), write(GP2),nl,
    !,
    grow(P, BNN, B1, GP2, EGP1). % In the future should be GP1
grow(P, B, B1, GP, EGP) :-
    write('Invalid position.'), nl,
    false.

