-module(poldercast).

-export[test/0].

test() ->
    start(?MODULE).

-spec start(atom()) -> ok | {error, term()}.
start(App) ->
    case application:start(App) of
        {error, {not_started, Dep}} ->
            ok = start(Dep),
            ok = start(App);
        Other ->
            Other
    end.
