-module(qna).

-export([
        upsert/1
    ]).

-type id() :: klsn:binstr().

-type qna() :: #{
        embe_id => embe:id()
      , embe_metadata => #{
            available => boolean()
          , deleted => boolean()
          , product => #{
                name => klsn:binstr()
              , version => klsn:binstr()
            }
          , sheat_id => klsn:binstr()
          , input => klsn:binstr()
          , title => [klsn:binstr()]
          , question => klsn:binstr()
          , note => [klsn:binstr()]
          , qna_id => id()
        }
      , embe_collection => atom() | klsn:binstr()
      , answer => klsn:binstr()
      , answer_sup => [klsn:binstr()]
      , state => init | embedded | answered | error
    }.

-spec normalize(qna()) -> qna().
normalize(Qna0) ->
    Qna = jsone:decode(jsone:encode(Qna0)),
    maps:remove(<<"_rev">>, maps:remove(<<"_id">>, Qna)).

-spec upsert(qna()) -> qna().
upsert(Qna0) ->
    Qna10 = normalize(Qna0),
    QnaId = case Qna10 of
        #{<<"embe_metadata">>:=#{<<"qna_id">>:=QnaId0}} ->
            QnaId0;
        _ ->
            embe_vector_db:uuid()
    end,
    Qna20 = klsn_map:upsert([<<"embe_metadata">>,<<"qna_id">>], QnaId, Qna10),
    Qna30 = case Qna20 of
        #{<<"state">>:=_} ->
            Qna20;
        _ ->
            Qna20#{<<"state">> => <<"init">>}
    end,
    Qna = Qna30,
    Res = klsn_db:upsert(?MODULE, QnaId, fun
        (none) ->
            Qna;
        ({value, Doc}) ->
            maps:merge(Doc, Qna)
    end),
    % to remove _id and _rev
    normalize(Res).

