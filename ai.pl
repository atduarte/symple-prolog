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
    N < 5,
    aiCreate(B, B1, P).
aiMove(B, B, 2, P, N) :-
    random(0, 10, X),
    X \= 0,
    aiGrow(B, B1, P).
aiMove(B, B1, 2, P, N) :-
    aiCreate(B, B1, P).
aiMove(B, B, 2, P, N) :-
    aiGrow(B, B1, P).

aiCreate(B, B1, P) :-
    getPossibleCreatePieces(B, P, PL), !,
    length(PL, S), !,
    S > 0, !,
    selectRandomItem(PL, I),
    pieceCreate(B, B1, P, I).

% TODO
aiGrow(B, B1, P).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%   Play  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
    X \= 0, !,
    append(R, [], RF).


