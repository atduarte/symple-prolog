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

checkAdjacent(B, C, L, P) :-
    C1 is C + 1,
    C2 is C - 1,
    L1 is L + 1,
    L2 is L - 1,
    checkAdjacentAuxL(B, C1, L, P, 1),
    checkAdjacentAuxL(B, C2, L, P, 1),
    checkAdjacentAuxL(B, C, L1, P, 1),
    checkAdjacentAuxL(B, C, L2, P, 1).

checkAdjacentAuxL([], C, L, P, NL).
checkAdjacentAuxL([H|T], C, L, P, NL) :-
    L = 0.
checkAdjacentAuxL([H|T], C, L, P, NL) :-
    C = 0.
checkAdjacentAuxL([H|T], C, L, P, NL) :-
    NL \= L,
    NL1 is NL + 1,
    checkAdjacentAuxL(T, C, L, P, NL1).
checkAdjacentAuxL([H|T], C, L, P, NL) :-
    NL = L, 
    checkAdjacentAuxC(H, C, P, 1).

checkAdjacentAuxC([], C, P, NC).
checkAdjacentAuxC([H|T], C, P, NC) :-
    C \= NC,
    NC1 is NC+1,
    checkAdjacentAuxC(T, C, P, NC1).
checkAdjacentAuxC([H|T], C, P, NC) :-
    C = NC,
    \+ checkPointPlayer(H, P).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%   Place Piece  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

placePiece(P, G, B, B1, C, L) :-
    length( B, S ),
    length( B1, S ),
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
    getNextGroupAux(B, P, GP),
    lastGroup(X, GP),
    G is X+1.

getNextGroupAux(B, 1, GP) :-
    getGroups(B, GP1, GP2),
    append(GP1, [], GP).
getNextGroupAux(B, 2, GP) :-
    getGroups(B, GP1, GP2),
    append(GP2, [], GP).


%%%%%%%%%%%%%%%%%%%%% Get Last Group %%%%%%%%%%%%%%%%%%%%%%%%%%%

lastGroup(0,[]).
lastGroup(X,[X]).
lastGroup(X,[_|L]) :- lastGroup(X,L).