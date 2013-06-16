#!/usr/bin/escript

-include("include/pdc_protocol.hrl").

recv(Sock) ->
    {ok, Packet} = gen_tcp:recv(Sock, 0),
    binary_to_term(Packet).

main(_) ->
    application:start(crypto),
    {ok, Sock} = gen_tcp:connect("127.0.0.1", 9999, 
                                 [binary, {packet, 2}, {active, false}]),
    Challenge = recv(Sock),

    Response = crypto:sha_mac_96(atom_to_list(erlang:get_cookie()), Challenge#pdc_auth_challenge.challenge),
    gen_tcp:send(Sock, term_to_binary(#pdc_auth_response{response=Response})),

    io:format("~p~n", [recv(Sock)]),

    ok.
