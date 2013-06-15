-module(pdc_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ranch:start_listener(pdc_tcp_listener, 1,
                         ranch_tcp,
                         [{port, get_env(port, 9999)}],
                         pdc_tcp, [{timeout, get_env(timeout, 5000)}]
                        ),

    pdc_sup:start_link().

stop(_State) ->
    ok.

-spec get_env(atom(), list() | integer()) -> list() | integer().
get_env(Field, Default) ->
    case application:get_env(poldercast, Field) of
        {ok, Value} -> Value;
        _ -> Default
    end.
