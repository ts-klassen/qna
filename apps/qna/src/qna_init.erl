-module(qna_init).

-export([
        db_setup/0
    ]).

db_setup() ->
    try klsn_db:create_db(qna_ip) catch
        error:exists -> ok
    end,
    try klsn_db:create_db(qna_user) catch
        error:exists -> ok
    end,
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
    ok.

