-module(qna_oplog).

-export([
        ok/3
      , error/5
    ]).

ok(_, _, _) ->
    ok.

error(_, _, Class, Error, Stack) ->
    spawn(fun()-> erlang:raise(Class, Error, Stack) end),
    ok.

