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

% TODO: Tem de retornar true se acabou
checkGameEnd(B) :-
    countFreeSpaces(B, C),
    !,
    C = 0.

countFreeSpaces([], 0).
countFreeSpaces([H|T], Count) :-
    countFreeSpacesAux(H, S1),
    countFreeSpaces(T, S2),
    Count is S1+S2.

countFreeSpacesAux([], 0).
countFreeSpacesAux([H|T], Count) :-
    checkIfPointFree(H),
    countFreeSpacesAux(T, S),
    Count is 1+S.
countFreeSpacesAux([H|T], Count) :-
    countFreeSpacesAux(T, S),
    Count is S.

checkIfPointFree([P|G]) :-
    P = 0.