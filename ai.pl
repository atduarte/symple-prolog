%%%%%%%%%%%%%%%%%%%%%%%%%%%%   Play  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Nivel 1
aiMove(B, B1, 1, P, N) :-
    random(0, 2, X),
    X = 0,
    aiCreate(B, B1, P).
aiMove(B, B1, 1, P, N) :-
    aiGrow(B, B1, P).
aiMove(B, B1, 1, P, N) :-
    aiCreate(B, B1, P).

% Nivel 2
aiMove(B, B1, 2, P, N) :-
    N > 5,
    aiCreate(B, B1, P).
aiMove(B, B1, 2, P, N) :-
    random(0, 10, X),
    X \= 0,
    aiGrow(B, B1, P).
aiMove(B, B1, 2, P, N) :-
    aiCreate(B, B1, P).
aiMove(B, B1, 2, P, N) :-
    aiGrow(B, B1, P).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%   Moves  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

aiCreate(B, B1, P) :-
    getPossibleCreatePieces(B, P, PL), !,
    length(PL, S), !,
    S > 0, !,
    selectRandomItem(PL, I),
    pieceCreate(B, B1, P, I).

aiGrow(B, B1, P) :-
    aiGrow(B, B1, P, [], 1).

aiGrow(B, B, P, EGP, N) :-
    getPlayerGroups(B, P, GP),
    length(GP, S1),
    length(EGP, S1),
    S1 > 0.
aiGrow(B, B1, P, EGP, N) :-
    getPlayerGroups(B, P, GP),
    length(GP, GPS),
    AX1 is GPS - N,
    GPS > 0,
    AX1 >= 0,
    getPossibleGrowPieces(B, P, EGP, PL),
    aiGrowAux(B, B1, P, EGP, N, 0, PL).

aiGrowAux(B, B, P, EGP, N, I, PL) :-
    length(PL, PLS),
    AX1 is PLS - I,
    AX1 < 0, !,
    false.
aiGrowAux(B, B1, P, EGP, N, I, PL) :-
    selectItem(PL, I, Point),
    pieceGrow(B, BN, P, EGP, EGP1, Point),
    N1 is N+1,
    aiGrow(BN, B1, P, EGP1, N1).
aiGrowAux(B, B1, P, EGP, N, I, PL) :-
    I1 is I+1, !,
    aiGrowAux(B, B1, P, EGP, N, I1, PL).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%   Auxiliars  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

getPossibleCreatePieces(B, P, R) :-
    getPossibleCreatePiecesAuxL(B, P, [], R, 1).

getPossibleCreatePiecesAuxL([], P, R, R, L).
getPossibleCreatePiecesAuxL([H|T], P, R, RF, L) :-
    getPossibleCreatePiecesAuxC(H, P, R, R1, L, 1),
    L1 is L+1,
    getPossibleCreatePiecesAuxL(T, P, R1, RF, L1).

getPossibleCreatePiecesAuxC([], P, R, R, L, C).
getPossibleCreatePiecesAuxC([H|T], P, R, RF, L, C) :-
    getPossibleCreatePiecesAuxP(H, P, R, R1, L, C),
    C1 is C+1,
    getPossibleCreatePiecesAuxC(T, P, R1, RF, L, C1).

getPossibleCreatePiecesAuxP([0, 0], P, R, RF, L, C) :-
    getAdjancentGroups(B, P, C, L, AGP),
    length(AGP, AGPS),
    AGPS = 0, !,
    append(R, [[L, C]], RF).
getPossibleCreatePiecesAuxP([X, Y], P, R, RF, L, C) :-
    append(R, [], RF).



getPossibleGrowPieces(B, P, EGP, R) :-
    getPossibleGrowPiecesAuxL(B, B, P, EGP, [], R, 1).

getPossibleGrowPiecesAuxL([], B, P, EGP, R, R, L).
getPossibleGrowPiecesAuxL([H|T], B, P, EGP, R, RF, L) :-
    getPossibleGrowPiecesAuxC(H, B, P, EGP, R, R1, L, 1),
    L1 is L+1,
    getPossibleGrowPiecesAuxL(T, B, P, EGP, R1, RF, L1).

getPossibleGrowPiecesAuxC([], B, P, EGP, R, R, L, C).
getPossibleGrowPiecesAuxC([H|T], B, P, EGP, R, RF, L, C) :-
    getPossibleGrowPiecesAuxP(H, B, P, EGP, R, R1, L, C),
    C1 is C+1,
    getPossibleGrowPiecesAuxC(T, B, P, EGP, R1, RF, L, C1).

getPossibleGrowPiecesAuxP([0, 0], B, P, EGP, R, RF, L, C) :-
    checkPointGetPossibleGrowPieces(B, P, C, L, EGP),
    append(R, [[L, C]], RF).
getPossibleGrowPiecesAuxP([X, Y], B, P, EGP, R, R, L, C).

checkPointGetPossibleGrowPieces(B, P, C, L, EGP) :-
    getAdjancentGroups(B, P, C, L, AGP),
    length(AGP, AGPS), !,
    AGPS > 0, !,
    checkNotExpanded(EGP, AGP).
