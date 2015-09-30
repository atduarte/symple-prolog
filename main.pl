%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Includes %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:-  use_module(library(random)),
    use_module(library(sockets)),
    consult(utilities),
    consult(board),
    consult(symple),
    consult(sockets),
    consult(ai).

user:runtime_entry(start) :-
    server.
