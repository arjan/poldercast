#!/usr/bin/escript

main(_) ->
    {ok, Sock} = gen_tcp:connect("127.0.0.1", 9999, 
                                 [binary, {packet, 2}, {active, false}]),
    ok = gen_tcp:send(Sock, term_to_binary({1,2,3, foo})),
    {ok, Packet} = gen_tcp:recv(Sock, 0),
    io:format("~p~n", [binary_to_term(Packet)]),

    ok = gen_tcp:close(Sock).
