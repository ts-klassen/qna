-module(qna_state).

-export([
        list/1
      , couchdb_views/0
      , states/0
    ]).

-spec list(klsn:binstr()) -> [qna:qna()].
list(Key) ->
    View = klsn_db:get(qna, {raw, <<"_design/qna_state/_view/", Key/binary, "?descending=true">>}),
    lists:map(fun(#{<<"value">>:=Value}) ->
        Value
    end, maps:get(<<"rows">>, View)).

-spec states() -> [klsn:binstr()].
states() ->
    [
        <<"init">>
      , <<"embedded">>
      , <<"searched">>
      , <<"ai_answered">>
      , <<"ai_unanswerable">>
      , <<"human_answered">>
      , <<"human_checked">>
      , <<"dataset">>
      , <<"excluded">>
      , <<"deleted">>
      , <<"error">>
      , <<"other">>
    ].

-spec couchdb_views() -> map().
couchdb_views() ->
    maps:from_list(lists:map(fun(Key) ->
        {Key, #{
            <<"map">> => <<"
            function (doc) {
                var state = doc.state ? doc.state : 'other';
                if (state == '", Key/binary, "')
                    emit(doc.U, doc);
            }
            ">>
        }}
    end, states())).

