%%%%%%%%%%%%%%%%%%%%%%%%%%%%   Change Player  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

checkBoardSize(S) :-
    S > 9, !,
    write('Choose a table size smaller than 10'), nl,
    false.
checkBoardSize(S) :-
    R is mod(S,2),
    R = 0, !,
    write('Choose a odd table size'),nl,
    false.
checkBoardSize(S) :-
    S < 10,
    R is mod(S,2),
    R = 1.

checkMode(M) :-
    M = 0.
checkMode(M) :-
    M = 1.
checkMode(M) :-
    M = 2.
checkMode(M) :-
    write('Invalid Mode.'),nl,
    false.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%   Change Player  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

changePlayer(P, P1) :-
    P = 1,
    P1 is 2.
changePlayer(P, P1) :-
    P = 2,
    P1 is 1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%   Check Place  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

checkPlace(B, C, L) :-
    checkPlaceAuxL(B, C, L, 1).

checkPlaceAuxL([H|T], C, L, NL) :-
    NL \= L,
    NL1 is NL + 1,
    checkPlaceAuxL(T, C, L, NL1).
checkPlaceAuxL([H|T], C, L, NL) :-
    NL = L, 
    checkPlaceAuxC(H, C, 1).

checkPlaceAuxC([H|T], C, NC) :-
    C \= NC,
    NC1 is NC+1,
    checkPlaceAuxC(T, C, NC1).
checkPlaceAuxC([H|T], C, NC) :-
    C = NC,
    checkPointPlayer(H, 0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%% Check Adjacent %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

checkNotAdjacent(B, C, L, P) :-
    C1 is C + 1,
    C2 is C - 1,
    L1 is L + 1,
    L2 is L - 1,
    checkNotAdjacentAuxL(B, C1, L, P, 1),
    checkNotAdjacentAuxL(B, C2, L, P, 1),
    checkNotAdjacentAuxL(B, C, L1, P, 1),
    checkNotAdjacentAuxL(B, C, L2, P, 1).

checkNotAdjacentAuxL([], C, L, P, NL).
checkNotAdjacentAuxL([H|T], C, L, P, NL) :-
    L = 0.
checkNotAdjacentAuxL([H|T], C, L, P, NL) :-
    C = 0.
checkNotAdjacentAuxL([H|T], C, L, P, NL) :-
    NL \= L,
    NL1 is NL + 1,
    checkNotAdjacentAuxL(T, C, L, P, NL1).
checkNotAdjacentAuxL([H|T], C, L, P, NL) :-
    NL = L, 
    checkNotAdjacentAuxC(H, C, P, 1).

checkNotAdjacentAuxC([], C, P, NC).
checkNotAdjacentAuxC([H|T], C, P, NC) :-
    C \= NC,
    NC1 is NC+1,
    checkNotAdjacentAuxC(T, C, P, NC1).
checkNotAdjacentAuxC([H|T], C, P, NC) :-
    C = NC,
    \+ checkPointPlayer(H, P).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%   Place Piece  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

placePiece(P, G, B, B1, C, L) :-
    placePieceAuxLine(P, G, B, B1, C, L, 1, 1).

placePieceAuxLine(P, G, [], [], C, L, NL, NC).
placePieceAuxLine(P, G, [H|T], [H|T1], C, L, NL, NC) :-
    NL \= L,
    NL1 is NL + 1,
    placePieceAuxLine(P, G, T, T1, C, L, NL1, NC).
placePieceAuxLine(P, G, [H|T], [H1|T1], C, L, NL, NC) :-
    NL = L,
    placePieceAuxColumn(P, G, H, H1, C, L, NC, NL),
    NL1 is NL + 1,
    placePieceAuxLine(P, G, T, T1, C, L, NL1, NC).

placePieceAuxColumn(P, G, [], [], C, L, NC, NL).
placePieceAuxColumn(P, G, [H|T], [H|T1], C, L, NC, NL) :-
    NC \= C,
    NC1 is NC + 1,
    placePieceAuxColumn(P, G, T, T1, C, L, NC1, NL).
placePieceAuxColumn(P, G, [H|T], [[P,G]|T1], C, L, NC, NL) :-
    NC = C,
    NC1 is NC + 1,
    placePieceAuxColumn(P, G, T, T1, C, L, NC1, NL).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%   Game End  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

checkGameEnd(B) :-
    countPieces(B, P1, P2, F),
    !,
    F = 0.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%  Calculate Points  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

calculatePoints(B, P1, P2) :-
    countPieces(B, PP1, PP2, PF),
    countGroups(B, PG1, PG2),
    P1 is PP1 - PG1,
    P2 is PP2 - PG2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%  Count Groups  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

countGroups(B, P1, P2) :-
    getGroups(B, GP1, GP2),
    length(GP1, P1),
    length(GP2, P2).

getGroups(B, GP1, GP2) :-
    getGroupsAuxL(B, [], [], GP1, GP2).

% each line
getGroupsAuxL([], GP1, GP2, GP1, GP2).
getGroupsAuxL([H|T], GP1, GP2, GP1F, GP2F) :-
    getGroupsAuxC(H, GP1, GP2, GP1N, GP2N),
    getGroupsAuxL(T, GP1N, GP2N, GP1F, GP2F).

% each column
getGroupsAuxC([], GP1, GP2, GP1, GP2). 
getGroupsAuxC([H|T], GP1, GP2, GP1F, GP2F) :-
    getGroupsAuxP(H, GP1, GP2, GP1N, GP2N),
    getGroupsAuxC(T, GP1N, GP2N, GP1F, GP2F).

% each Point
getGroupsAuxP([0, G], GP1, GP2, GP1, GP2).
getGroupsAuxP([1, G], GP1, GP2, GP1F, GP2) :-
    appendIfNotDuplicate(GP1, G, GP1F).
getGroupsAuxP([2, G], GP1, GP2, GP1, GP2F) :-
    appendIfNotDuplicate(GP2, G, GP2F).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%  Count Pieces  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

countPieces([], 0, 0, 0).
countPieces([H|T], P1, P2, F) :-
    countPiecesAux(H, P1H, P2H, FH),
    countPieces(T, P1T, P2T, FT),
    P1 is P1H+P1T,
    P2 is P2H+P2T,
    F  is FH+FT.

countPiecesAux([], 0, 0, 0).
countPiecesAux([H|T], P1, P2, F) :-
    checkPointPlayer(H, 0),
    countPiecesAux(T, P1, P2, FT),
    F  is 1+FT.
countPiecesAux([H|T], P1, P2, F) :-
    checkPointPlayer(H, 1),
    countPiecesAux(T, P1T, P2, F),
    P1 is 1+P1T.
countPiecesAux([H|T], P1, P2, F) :-
    checkPointPlayer(H, 2),
    countPiecesAux(T, P1, P2T, F),
    P2 is 1+P2T.

checkPointPlayer([P|G], P1) :-
    P = P1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%  Other  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

appendIfNotDuplicate(L, X, L) :-
    member(X, L).
appendIfNotDuplicate(L, X, L1) :-
    append(L, [X], L1).

%%%%%%%%%%%%%%%%%%%%%%  Get Next Group  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

getNextGroup(B, P, G) :-
    getPlayerGroups(B, P, GP),
    lastGroup(X, GP),
    G is X+1.

getPlayerGroups(B, 1, GP) :-
    getGroups(B, GP, GP2).
getPlayerGroups(B, 2, GP) :-
    getGroups(B, GP1, GP).


%%%%%%%%%%%%%%%%%%%%% Get Last Group %%%%%%%%%%%%%%%%%%%%%%%%%%%

lastGroup(0,[]).
lastGroup(X,[X]).
lastGroup(X,[_|L]) :- lastGroup(X,L).

%%%%%%%%%%%%%%%%%%%%% Get Last Group %%%%%%%%%%%%%%%%%%%%%%%%%%%

askPosition(C, L) :-
    write('Choose column to place'),
    read(C),
    write('Choose line to place'),
    read(L).

%%%%%%%%%%%%%%%%%%%%% Get Adjacent Groups %%%%%%%%%%%%%%%%%%%%%%%%%%%

getAdjancentGroups(B, P, C, L, AGP) :-
    C1 is C + 1,
    C2 is C - 1,
    L1 is L + 1,
    L2 is L - 1,
    getAdjancentGroupsAuxL(B, P, C1, L, 1, [], AGP0),
    getAdjancentGroupsAuxL(B, P, C2, L, 1, AGP0, AGP1),
    getAdjancentGroupsAuxL(B, P, C, L1, 1, AGP1, AGP2),
    getAdjancentGroupsAuxL(B, P, C, L2, 1, AGP2, AGP),
    true.

getAdjancentGroupsAuxL([],    P, C, L, NL, AGP, AGP).
getAdjancentGroupsAuxL([H|T], P, C, L, NL, AGP, AGP) :-
    C = 0.
getAdjancentGroupsAuxL([H|T], P, C, L, NL, AGP, AGP) :-
    L = 0.
getAdjancentGroupsAuxL([H|T], P, C, L, NL, AGP, AGPF) :-
    NL \= L,
    NL1 is NL + 1,
    getAdjancentGroupsAuxL(T, P, C, L, NL1, AGP, AGPF).
getAdjancentGroupsAuxL([H|T], P, C, L, NL,  AGP, AGPF) :-
    NL = L, 
    getAdjancentGroupsAuxC(H, P, C, 1, AGP, AGPF).

getAdjancentGroupsAuxC([],    P, C, NC, AGP, AGP).
getAdjancentGroupsAuxC([H|T], P, C, NC, AGP, AGPF) :-
    NC \= C,
    NC1 is NC+1, 
    getAdjancentGroupsAuxC(T, P, C, NC1, AGP, AGPF).
getAdjancentGroupsAuxC([H|T], P, C, NC, AGP, AGPF) :-
    NC = C,
    checkPointPlayer(H, P), 
    getPointGroup(H, G),
    append(AGP, [G], AGPF).
getAdjancentGroupsAuxC([H|T], P, C, NC, AGP, AGP) :-
    NC = C.


%%%%%%%%%%%%%%%%%%%%% Get Point Info %%%%%%%%%%%%%%%%%%%%%%%%%%%

getPointPlayer([P,G], P).
getPointGroup([P,G], G).

myLength([], 0).
myLength([H|T], S) :-
    myLength(T, S1),
    S is 1+S1.

checkNotExpanded(EGP, []).
checkNotExpanded(EGP, [H|T]) :-
    \+ member(H, EGP),
    checkNotExpanded(EGP, T).

changeGroups(B, BF, P, AGP, G):-
    changeGroupsAuxL(B, BF, P, AGP, G).

% each line
changeGroupsAuxL([], [], P, AGP, G).
changeGroupsAuxL([H|T], [H1|T1], P,  AGP, G) :-
    changeGroupsAuxC(H, H1, P, AGP, G),
    changeGroupsAuxL(T, T1, P, AGP, G).

% each column
changeGroupsAuxC([], [], P, AGP, G).
changeGroupsAuxC([H|T], [H1|T1], P,  AGP, G) :-
    changeGroupsAuxP(H, H1, P, AGP, G),
    changeGroupsAuxC(T, T1, P, AGP, G).

% each Point
changeGroupsAuxP([P, Y], [P, G], P, AGP, G) :-
    member(Y, AGP).
changeGroupsAuxP([X, Y], [X, Y], P, AGP, G).


selectRandomItem(L, I) :-
    length(L, S),
    random(0, S, X),
    selectItem(L, X, I).

selectItem(B, X, I) :-
    selectItem(B, X, I, 0).
selectItem([H|T], X, H, N) :-
    X = N.
selectItem([H|T], X, I, N) :-
    N1 is N+1,
    selectItem(T, X, I, N1).


