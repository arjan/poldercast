-module(pdc_tcp).
-behaviour(gen_server).

-record(state,
        {
          socket :: port(),
          transport :: atom(),
          listener_pid :: pid(),
          link_pid :: pid(),
          timeout :: integer()
        }).

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

start_link(ListenerPid, Socket, Transport, Opts) ->
	gen_server:start_link(?MODULE, [ListenerPid, Socket, Transport, Opts], []).

-spec close(pid(), atom()) -> ok | error.
close(Pid, Reason) ->
    gen_server:call(Pid, {close, Reason}).

-spec send(pid(), term()) -> ok.
send(Pid, Message) ->
    gen_server:call(Pid, {send, Message}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([ListenerPid, Socket, Transport, [{timeout, Timeout}]]) ->
    {ok, #state{socket=Socket,
                listener_pid=ListenerPid,
                transport=Transport,
                timeout=Timeout}}.


handle_call({close, _}, _From, State=#state{socket=undefined}) ->
    {stop, normal, ok, State};
handle_call({close, Reason}, _From, State) ->
    Packet = case Reason of
                 {reply, R} -> getaroom_tcp_msg:reply(R);
                 {error, R} -> getaroom_tcp_msg:error(R)
             end,
    {stop, normal, ok, close_connection(Packet, State)};


%% handle_call({set_socket, Socket}, _From, State) ->
%%     {reply, ok, State#state{socket=Socket}};

handle_call({send, Message}, _From, State) ->
    %% FIXME encode here?
    send_and_reply(Message, ok, State);

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


%% TCP message handling

handle_info({tcp_closed, Socket}, State=#state{socket=Socket}) ->
    {stop, normal, State};

handle_info({tcp, Socket, Line}, State=#state{socket=Socket}) ->
    do_line_received(Line, State);


%% SSL message handling

handle_info({ssl_closed, Socket}, State=#state{socket=Socket}) ->
    {stop, normal, State};

handle_info({ssl, Socket, Line}, State=#state{socket=Socket}) ->
    do_line_received(Line, State);


%% @doc Ranch callback when socket ready
handle_info({shoot, Pid}, State=#state{listener_pid=Pid,
                                       socket=Socket,
                                       transport=Transport}) ->
    Transport:setopts(Socket, [{active, true}, {packet, line}, {recbuf, ?BUF_SIZE}, binary]),
    {noreply, State};
             
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

handle_incoming({cmd, <<"ping">>, _Args}, State) ->
    send_or_throw(getaroom_tcp_msg:reply(<<"pong">>), State),
    State;

handle_incoming({cmd, <<"login">>=Cmd, Args}, State=#state{link_pid=undefined}) ->
    case getaroom_user:login(self(), Args) of
        {ok, {Pid, U}} ->
            send_or_throw(getaroom_tcp_msg:reply(Cmd, [{user, getaroom_user_util:user_to_json(U)}]), State),
            true = erlang:link(Pid),
            State#state{link_pid=Pid};
        {error, Reason} ->
            send_or_throw(getaroom_tcp_msg:error(Reason), State),
            State
    end;

handle_incoming({cmd, <<"login">>, _Args}, #state{}) ->
    throw({error, <<"Already logged in">>});

handle_incoming({cmd, <<"logout">>, _Args}, #state{link_pid=UserPid}) when is_pid(UserPid) ->
    throw(logout);


%% @doc When logged in, handle all commands in the user process.
handle_incoming({cmd, Cmd, Args}, State=#state{link_pid=UserPid}) when is_pid(UserPid) ->
    Response =
        case getaroom_user_worker:user_command(UserPid, Cmd, Args) of
            {ok, Reply} ->
                getaroom_tcp_msg:reply(Cmd, Reply);
            {error, Reason} ->
                getaroom_tcp_msg:error(Reason)
        end,
    send_or_throw(Response, State),
    State;

handle_incoming({cmd, C, _Args}, State) ->
    send_or_throw(getaroom_tcp_msg:error(<<"Invalid or unknown command: ", C/binary>>), State),
    State;

handle_incoming({_, _, _Args}, State) ->
    close_connection(getaroom_tcp_msg:error(<<"Protocol error.">>), State).


%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

close_connection(ExitMessage, State=#state{socket=Socket, transport=Transport}) ->
    ok = Transport:send(Socket, ExitMessage),
    ok = Transport:close(Socket),
    State#state{socket=undefined}.
    

%% @doc Handle the receive of a line.
do_line_received(Line, State) ->
    try
        {ok, Message} = getaroom_tcp_msg:parse(Line),
        {noreply, handle_incoming(Message, State)}
    catch
        throw:closed ->
            {stop, normal, State};
        throw:logout ->
            {stop, normal, close_connection(getaroom_tcp_msg:reply(ok), State)};
        throw:{error, Reason} ->
            send_and_noreply(getaroom_tcp_msg:error(Reason), State)
    end.

do_send(Message, #state{socket=Socket, transport=Transport}) ->
    Transport:send(Socket, Message).

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
    case send(Message, State) of
        ok ->
            {noreply, State};
        {error, closed} ->
            {stop, normal, State}
    end.
    
