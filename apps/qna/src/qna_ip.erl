-module(qna_ip).

-export([
        lookup/1
      , new/2
      , list/0
      , from_tuple/1
    ]).

-type new_opt() :: #{
        memo => boolean()
      , created_by => qna_user:user()
    }.

-type ip() :: klsn:binstr().

-type info() :: map().

-spec list() -> [ip()].
list() ->
    #{<<"rows">>:=Rows} = klsn_db:get(?MODULE, {raw, <<"_design/qna_ip/_view/list">>}),
    lists:map(fun
        (#{<<"id">>:=Id, <<"value">>:=Value}) ->
            Value
    end, Rows).

-spec lookup(ip()) -> klsn:maybe(info()).
lookup(Ip) ->
    klsn_db:lookup(?MODULE, Ip).


-spec new(ip(), new_opt()) -> ok.
new(Ip, Info) ->
    klsn_db:upsert(?MODULE, Ip, fun
        ({value, _}) ->
            erlang:error(exists);
        (none) ->
            Info#{ ip => Ip }
    end),
    ok.

from_tuple({_, _, _, _}=Ip) ->
    iolist_to_binary(lists:join(<<".">>, lists:map(fun erlang:integer_to_binary/1, tuple_to_list(Ip)))).

