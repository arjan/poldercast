-module(pdc_tcp).
-behaviour(gen_server).

-record(state,
        {
          socket :: port(),
          transport :: atom(),
          ranch_ref :: pid(),
          link_pid :: pid(),
          timeout :: integer(),

          auth_challenge :: binary()
        }).

-include_lib("../include/pdc_protocol.hrl").

-define(BUF_SIZE, 1024).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/4,
         close/2,
         send/2
        ]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Ref, Socket, Transport, Opts) ->
	gen_server:start_link(?MODULE, [Ref, Socket, Transport, Opts], []).

-spec close(pid(), atom()) -> ok | error.
close(Pid, Reason) ->
    gen_server:call(Pid, {close, Reason}).

-spec send(pid(), term()) -> ok.
send(Pid, Message) ->
    gen_server:call(Pid, {send, Message}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Ref, Socket, Transport, [{timeout, Timeout}]]) ->
    {ok, #state{socket=Socket,
                ranch_ref=Ref,
                transport=Transport,
                timeout=Timeout}, 0}.


handle_call({close, _}, _From, State=#state{socket=undefined}) ->
    {stop, normal, ok, State};
handle_call({close, Reason}, _From, State) ->
    Packet = encode(#pdc_closed{reason=Reason}),
    {stop, normal, ok, close_connection(Packet, State)};


%% handle_call({set_socket, Socket}, _From, State) ->
%%     {reply, ok, State#state{socket=Socket}};

handle_call({send, Message}, _From, State) ->
    send_and_reply(Message, ok, State);

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


%% @doc Initial start of socket flow
handle_info(timeout, State=#state{ranch_ref=Ref,
                                  socket=Socket,
                                  transport=Transport}) ->
    
    ok = ranch:accept_ack(Ref),
    ok = Transport:setopts(Socket, [{active, once}, {packet, 2}, {recbuf, ?BUF_SIZE}, binary]),
    AuthChallenge = crypto:rand_bytes(64),
    send_and_noreply(
      #pdc_auth_challenge{challenge=AuthChallenge},
      State#state{auth_challenge=AuthChallenge});


%% TCP message handling

handle_info({tcp_closed, Socket}, State=#state{socket=Socket}) ->
    {stop, normal, State};

handle_info({tcp, Socket, Packet}, State=#state{socket=Socket}) ->
    do_packet_received(Packet, State);


%% SSL message handling

handle_info({ssl_closed, Socket}, State=#state{socket=Socket}) ->
    {stop, normal, State};

handle_info({ssl, Socket, Packet}, State=#state{socket=Socket}) ->
    do_packet_received(Packet, State);


handle_info(Info, State) ->
    lager:warning("Unhandled message: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ------------------------------------------------------------------
%% Handle protocol commands
%% ------------------------------------------------------------------

handle_incoming(#pdc_auth_response{protocol_version=ClientVersion}, State) when ClientVersion =/= ?PROTOCOL_VERSION ->
    error_and_stop(<<"Protocol version mismatch">>, State);

handle_incoming(#pdc_auth_response{response=Response}, State=#state{auth_challenge=AuthChallenge}) ->
    Key = pdc_app:get_auth_shared_secret(),
    case crypto:sha_mac_96(Key, AuthChallenge) of
        Response ->
            %% FIXME do stuff here, like registering this link with the link manager.
            error_and_stop(<<"Not implemented">>, State);
        _ ->
            error_and_stop(<<"Auth challenge failure">>, State)
    end;

handle_incoming(_, State=#state{link_pid=undefined}) ->
    send_and_noreply(#pdc_error{reason= <<"Authenticate first">>}, State);

handle_incoming(_UnknownTerm, State) ->
    error_and_stop(<<"Protocol error">>, State).


%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

close_connection(ExitMessage, State=#state{socket=Socket, transport=Transport}) ->
    do_send(ExitMessage, State),
    ok = Transport:close(Socket),
    State#state{socket=undefined}.
    

%% @doc Handle the receive of a line.
do_packet_received(Packet, State=#state{socket=Socket, transport=Transport}) ->
    try
        Message = decode(Packet),
        handle_incoming(Message, State)
    catch
        throw:closed ->
            {stop, normal, State};
        throw:{error, Reason} ->
            send_and_noreply(#pdc_error{reason=Reason}, State)
    end.

do_send(Message, #state{socket=Socket, transport=Transport}) when is_tuple(Message) ->
    Packet = encode(Message),
    Transport:send(Socket, Packet),
    Transport:setopts(Socket, [{active, once}]).

send_or_throw(Message, State) ->
    case do_send(Message, State) of
        ok ->
            ok;
        {error, closed} ->
            throw(closed)
    end.
            
send_and_reply(Message, Reply, State) ->
    case do_send(Message, State) of
        ok ->
            {reply, Reply, State};
        {error, closed} ->
            {stop, Reply, normal, State}
    end.

send_and_noreply(Message, State) ->
    case do_send(Message, State) of
        ok ->
            {noreply, State};
        {error, closed} ->
            {stop, normal, State}
    end.
    
error_and_stop(Reason, State) ->
  {stop, normal, close_connection(#pdc_error{reason=Reason}, State)}.


%% @doc Encode a #pdc_..{} record as TCP packet payload
-spec encode(tuple()) -> iolist().
encode(Message) when is_tuple(Message) ->
    term_to_binary(Message).
    
-spec decode(binary()) -> tuple().
decode(Payload) when is_binary(Payload) ->
    binary_to_term(Payload).
    
