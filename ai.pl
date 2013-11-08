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
aiMove(B, B1, 2, P, N) :-
    aiGrow(B, B1, P).
aiMove(B, B1, 2, P, N) :-
    aiCreate(B, B1, P).

% TODO
aiCreate(B, B1, P).

% TODO
aiGrow(B, B1, P).

