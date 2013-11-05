%%%%%%%%%%%%%%%%%%%%%%%%%%%%   Change Player  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

changePlayer(P, P1) :-
    P = 1,
    P1 is 2.
changePlayer(P, P1) :-
    P = 2,
    P1 is 1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%   Place Piece  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

placePiece(P, B, B1, C, L) :-
    length( B, S ),
    length( B1, S ),
    placePieceAuxLine(P, B, B1, C, L, 1, 1).

% TODO: Change B1 next to B

placePieceAuxLine(P, [], [], C, L, NL, NC).
placePieceAuxLine(P, [H|T], [H|T1], C, L, NL, NC) :-
    NL \= L,
    NL1 is NL + 1,
    placePieceAuxLine(P, T, T1, C, L, NL1, NC).
placePieceAuxLine(P, [H|T], [H1|T1], C, L, NL, NC) :-
    NL = L,
    placePieceAuxColumn(P, H, H1, C, L, NC, NL),
    NL1 is NL + 1,
    placePieceAuxLine(P, T, T1, C, L, NL1, NC).

placePieceAuxColumn(P, [], [], C, L, NC, NL).
placePieceAuxColumn(P, [H|T], [H|T1], C, L, NC, NL) :-
    NC \= C,
    NC1 is NC + 1,
    placePieceAuxColumn(P, T, T1, C, L, NC1, NL).
placePieceAuxColumn(P, [H|T], [[P,0]|T1], C, L, NC, NL) :-
    NC = C,
    NC1 is NC + 1,
    placePieceAuxColumn(P, T, T1, C, L, NC1, NL).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%   Game End  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

checkGameEnd(B) :-
    countPieces(B, P1, P2, F),
    !,
    F = 0.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%  Calculate Points  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

calculatePoints(B, P1, P2) :-
    countSpaces(B, PP1, PP2, PF),
    countGroups(B, PG1, PG2),
    P1 is PP1 - PG1,
    P2 is PP2 - PG2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%  Count Groups  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

countGroups(B, P1, P2) :-
    countGroupsAuxL(B, [], [], GP1, GP2),
    length(GP1, P1),
    length(GP2, P2).

% each line
countGroupsAuxL([], GP1, GP2, GP1, GP2).
countGroupsAuxL([H|T], GP1, GP2, GP1F, GP2F) :-
    countGroupsAuxC(H, GP1, GP2, GP1N, GP2N),
    countGroupsAuxL(T, GP1N, GP2N, GP1F, GP2F).

% each column
countGroupsAuxC([], GP1, GP2, GP1, GP2). 
countGroupsAuxC([H|T], GP1, GP2, GP1F, GP2F) :-
    countGroupsAuxP(H, GP1, GP2, GP1N, GP2N),
    countGroupsAuxC(T, GP1N, GP2N, GP1F, GP2F).

% each Point
countGroupsAuxP([0, G], GP1, GP2, GP1, GP2).
countGroupsAuxP([1, G], GP1, GP2, GP1F, GP2) :-
    appendIfNotDuplicate(GP1, G, GP1F).
countGroupsAuxP([2, G], GP1, GP2, GP1, GP2F) :-
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

appendIfNotDuplicate(L, X, L1) :-
    member(X, L),
    append(L, [], L1).
appendIfNotDuplicate(L, X, L1) :-
    append(L, [X], L1).