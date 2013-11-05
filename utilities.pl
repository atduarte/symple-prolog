%%%%%%%%%%%%%%%%%%%%%%%%%%%%   Change Player  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

changePlayer(P, P1) :-
    P = 1,
    P1 is 2.
changePlayer(P, P1) :-
    P = 2,
    P1 is 1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%   Place Piece  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

placePiece(P, B, B1, C, L) :-
    NL is 1,
    NC is 1,
    length( B, S ),
    length( B1, S ),
    placePieceAuxLine(P, B, C, L, B1, NL, NC).

% TODO: Change B1 next to B

placePieceAuxLine(P, [], C, L, [], NL, NC).
placePieceAuxLine(P, [H|T], C, L, [H1|T1], NL, NC) :-
    placePieceAuxColumn(P, H, C, L, H1, NC, NL),
    NL1 is NL + 1,
    placePieceAuxLine(P, T, C, L, T1, NL1, NC).


placePieceAuxColumn(P, [], C, L, [], NC, NL).
placePieceAuxColumn(P, [H|T], C, L, [H1|T1], NC, NL) :-
    NC \= C,
    %write('NC:'), write(NC), nl, % DEGUB
    append(H, [], H1),
    NC1 is NC + 1,
    placePieceAuxColumn(P, T, C, L, T1, NC1, NL).
placePieceAuxColumn(P, [H|T], C, L, [H1|T1], NC, NL) :-
    NC = C,
    NL \= L,
    %write('NC:'), write(NC), nl, % DEGUB
    append(H, [], H1),
    NC1 is NC + 1,
    placePieceAuxColumn(P, T, C, L, T1, NC1, NL).
placePieceAuxColumn(P, [H|T], C, L, [H1|T1], NC, NL) :-
    NC = C,
    NL = L,
    %write('Write NC:'), write(NC), nl, % DEGUB
    append([P,0], [], H1),
    NC1 is NC + 1,
    placePieceAuxColumn(P, T, C, L, T1, NC1, NL).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%   Game End  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% TODO: Tem de retornar true se acabou
checkGameEnd(B) :-
    false.