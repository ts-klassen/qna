-module(qna_init).

-export([
        db_setup/0
    ]).

db_setup() ->
    lists:map(fun(DB) ->
        try klsn_db:create_db(DB) catch
            error:exists -> ok
        end
    end, [qna_id, qna_user, qna_ai, qna]),
    embe:init_setup(),
    klsn_db:upsert(qna_ip, <<"_design/qna_ip">>, fun(MaybeDoc) ->
        Doc = case MaybeDoc of
            {value, Doc0} -> Doc0;
            none -> #{}
        end,
        Doc#{
            <<"views">> => #{
                <<"list">> => #{
                    <<"map">> => <<"function (doc) {emit(doc.C, {ip: doc._id, memo: doc.memo, created_by: doc.created_by, created_at: doc.C});}">>
                }
            }
          , <<"language">> => <<"javascript">>
        }
    end),
    klsn_db:upsert(qna_user, <<"_design/qna_user">>, fun(MaybeDoc) ->
        Doc = case MaybeDoc of
            {value, Doc0} -> Doc0;
            none -> #{}
        end,
        Doc#{
            <<"views">> => #{
                <<"list">> => #{
                    <<"map">> => <<"function (doc) {emit(doc.C, {id: doc._id, is_admin: doc.is_admin, created_by: doc.created_by, created_at: doc.C});}">>
                }
            }
          , <<"language">> => <<"javascript">>
        }
    end),
    klsn_db:upsert(qna_ai, <<"_design/qna_ai">>, fun(MaybeDoc) ->
        Doc = case MaybeDoc of
            {value, Doc0} -> Doc0;
            none -> #{}
        end,
        Doc#{
            <<"views">> => #{
                <<"cost">> => #{
                    <<"map">> => qna_ai:calc_cost_js()
                  , <<"reduce">> => <<"_sum">>
                }
            }
          , <<"language">> => <<"javascript">>
        }
    end),
    ok.

