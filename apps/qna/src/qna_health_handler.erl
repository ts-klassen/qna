-module(qna_health_handler).
-behavior(cowboy_rest).

-export([init/2]).

init(Req0, State) ->
    Req = cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/plain">>
    }, <<"1\n">>, Req0),
    io:format("~p~n", [Req]),
    {ok, Req, State}.
