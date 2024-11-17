-module(qna_init).

-export([
        db_setup/0
    ]).

db_setup() ->
    klsn_db:create_db(qna_ip),
    klsn_db:create_db(qna_user),
    embe:init_setup(),
    ok.

