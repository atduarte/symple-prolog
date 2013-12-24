% Sockets Galo - Luis Paulo Reis, 16 de Novembro de 2004 - corrigido para SICStus4 em Novembro de 2009

% Parte 1: Utilizacao generica de sockets para um jogo de tabuleiro

:-use_module(library(sockets)).

port(60001).

server:-
    port(Port),
    socket_server_open(Port,Socket),
    socket_server_accept(Socket, _Client, Stream, [type(text)]),
    server_loop(Stream),
    socket_server_close(Socket),
    write('Server Exit'),nl.

server_loop(Stream) :-
    repeat,
        read(Stream, ClientRequest),
        write('Received: '), write(ClientRequest), nl,
        server_input(ClientRequest, ServerReply),
        format(Stream, '~q.~n', [ServerReply]),
        write('Send: '), write(ServerReply), nl,
        flush_output(Stream),
    (ClientRequest == bye; ClientRequest == end_of_file), !.


server_input([create, Board, Player, Column, Row ], [NBoard]) :-
    pieceCreate(Board, NBoard, Player, Column, Row),!.
server_input([create, Board, Player, Column, Row], 0) :- !.


server_input([grow, Board, Player, Points], [NBoard]) :-
    ( foreach(Point, Points),
      param(Player),
      fromto(Board, In, Out, NBoard),
      fromto([], EGP, NEGP, FEGP)
      do
        pieceGrow(In, Out, Player, EGP, NEGP, Point)
    ),!.
server_input([grow, Board, Player, Points], 0) :- !.


server_input([playAi, Board, Player, Level], [NBoard]) :-
    aiMove(Board, NBoard, Level, Player, 1),!.
server_input([playAi, Board, Player, Level], 0) :- !.


server_input([checkGameEnd, Board], 1) :-
    checkGameEnd(Board), !.
server_input([checkGameEnd, Board], 0) :- !.


server_input(bye, ok) :- !.
server_input(end_of_file, ok) :- !.
server_input(_, invalid) :- !.
