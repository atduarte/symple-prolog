%%%%%%%%%%%%%%%%%%%%%%%%%%%  Init Play Symple  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

play :- 
    askBoardSize(S),
    askMode(MD),
    playInit(S, MD).

play(S, MD) :- 
    playInit(S, MD).

playInit(S, MD) :-
    createBoard(B, S),
    printBoard(B),
    P is 1,
    !,
    playerPlay(B, MD, P, 0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%  INIT - Ask Settings  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

askBoardSize(S) :-
    write('Choose board size: '),
    read(S),
    checkBoardSize(S).
askBoardSize(S) :-
    askBoardSize(S).

askMode(MD) :-
    write('Choose mode: '), nl,
    write('(0 - Person vs Person)'), nl,
    write('(1 - Person vs Easy AI)'), nl,
    write('(2 - Person vs Difficult AI)'), nl,
    read(MD),
    checkMode(MD).
askMode(MD) :-
    askMode(MD).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%   Player Play  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Verifica se acabou
playerPlay(B, MD, P, N) :-
    checkGameEnd(B),
    write('Game Ended.'), nl,
    countPieces(B, P1, P2, F),
    write('Player 1: '), write(P1), write(' points.'),nl,
    write('Player 2: '), write(P2), write(' points.'),nl.
% Se for AI 1
playerPlay(B, 1, 2, N) :-
    write('AI 1'), write(N), nl,
    aiMove(B, B1, 1, 2, N),
    N1 is N+1,
    playerPlayEnd(B1, 1, 2, N1).
% Se for AI 2
playerPlay(B, 2, 2, N) :-
    write('AI 2'),nl,
    aiMove(B, B1, 2, 2, N),
    N1 is N+1,
    playerPlayEnd(B1, 2, 2, N1).
% Se for Player 1 - Manual
playerPlay(B, MD, P, N) :-
    P = 1,
    askManualMove(B, B1, P),
    playerPlayEnd(B1, MD, P, N).
% Se for Player 2 - Manual
playerPlay(B, MD, P, N) :-
    MD = 0,
    P = 2,
    askManualMove(B, B1, P),
    playerPlayEnd(B1, MD, P, N).
% Repeat
playerPlay(B, MD, P, N) :- playerPlay(B, MD, P, N).

playerPlayEnd(B, MD, P, N) :-
    printBoard(B),
    changePlayer(P, P1),
    playerPlay(B, MD, P1, N).
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%   Manual Moves  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

askManualMove(B, B1, P) :-
    write('PLAYER '),
    write(P),
    write(', Your turn! Choose move type (create/grow)'),
    read(M),
    manualMove(P, B, B1, M).
askManualMove(B, B1, P) :- askManualMove(B, B1, P).

% INVALID MOVE
manualMove(P, B, B1, M) :-
    M \= 'grow',
    M \= 'create', 
    !,
    write('Invalid Move'), nl,
    false.
    
% GROW
manualMove(P, B, B1, 'grow') :-
    getExpandableGroups(B, P, GP), !,
    checkGrow(GP), !,
    manualGrowAux(B, B1, P, GP, []).

manualGrowAux(B, B, P, GP, EGP) :-
    checkAllExpanded(EGP, GP),
    %length(GP, S1),
    %length(EGP, S2),
    %write('S1: '), write(S1), nl,
    %write('S2: '), write(S2), nl,
    %S2 >= S1,
    write('Grow End').
manualGrowAux(B, B1, P, GP, EGP) :-
    write('Grow'), write(EGP), nl,

    getExpandableGroups(B, P, GPN),
    write('Expandable: '), write(GPN), nl,
    write('Expanded: '), write(EGP), nl,

    askPosition(C, L),
    pieceGrow(B, BN, P, EGP, EGP1, C, L), !,

    getExpandableGroups(BN, P, GP1),
    write('Expandable: '), write(GP1), nl,
    write('Expanded: '), write(EGP1), nl,

    manualGrowAux(BN, B1, P, GP1, EGP1).
manualGrowAux(B, B1, P, GP, EGP) :-
    write('Invalid position.'), nl, !,
    false.
    

% CREATE
manualMove(P, B, B1, 'create') :-
    askPosition(C, L),
    pieceCreate(B, B1, P, C, L).
manualMove(P, B, B1, 'create') :-
    write('Invalid position.'), nl,
    false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%   Moves  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pieceCreate(B, B1, P, [L, C]) :-
    pieceCreate(B, B1, P, C, L).
pieceCreate(B, B1, P, C, L) :-
    checkPlace(B, C, L),
    checkNotAdjacent(B, C, L, P),
    getNextGroup(B, P, G), !,
    placePiece(P, G, B, B1, C, L).

pieceGrow(B, B1, P, EGP, EGP1, [L, C]) :-
    pieceGrow(B, B1, P, EGP, EGP1, C, L).
pieceGrow(B, B1, P, EGP, EGP1, C, L) :-
    % Verificar se posição está livre
    checkPlace(B, C, L), !,
 
    % Procurar grupos adjacentes
    getAdjancentGroups(B, P, C, L, AGP),
    % Verificar que existem grupos adjacentes
    length(AGP, AGPS),
    %write('DEBUG AGPS:'), write(AGPS), write(AGP), nl,
    AGPS > 0, !,
 
    % Verificar que grupos adjacentes não foram já expandidos
    %write('DEGUG: '), write(EGP), write(AGP), nl,
    checkNotExpanded(EGP, AGP), !,
 
    % Mudar todos os pontos dos grupos adjacentes para um novo grupo
    % E mudar lista de grupos
    getNextGroup(B, P, G),
    changeGroups(B, BN, P, AGP, G),
 
    % Colocar peça
    placePiece(P, G, BN, B1, C, L),
 
    % Criar nova lista de grupos expandidos
    append(EGP, [G], EGP1).

checkGrow(GP) :-
    length(GP, X),
    X = 0, !,
    write('Invalid: You dont have groups to grow'), nl,
    false.
checkGrow(GP).