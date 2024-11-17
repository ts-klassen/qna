-module(qna_user).

-export([
        lookup/1
      , new/2
      , random_pw/0
      , lookup_by_auth_header/1
      , update_pw/3
      , list/0
    ]).

-export_type([
        new_opt/0
      , user_id/0
      , user/0
    ]).

-type new_opt() :: #{
        is_admin := boolean()
      , created_by => klsn:binstr()
    }.

-type user_id() :: klsn:binstr().

-type user() :: map().

-spec list() -> [user()].
list() ->
    #{<<"rows">>:=Rows} = klsn_db:get(?MODULE, <<"_all_docs">>),
    lists:map(fun(#{<<"id">>:=Id}) ->
        Id
    end, Rows).

-spec lookup(user_id()) -> klsn:maybe(user()).
lookup(User) ->
    klsn_db:lookup(?MODULE, User).

-spec lookup_by_auth_header(klsn:binstr()) -> klsn:maybe(user()).
lookup_by_auth_header(AuthorizationHeader) ->
    {Username, Password} = credentials_from_header(AuthorizationHeader),
    case qna_user:lookup(Username) of
        {value, #{<<"bcrypt">>:=Bcrypt}=Doc} ->
            case bcrypte:verify(Password, Bcrypt) of
                true ->
                    {value, Doc};
                false ->
                    none
            end;
        none ->
            none
    end.

-spec new(user_id(), new_opt()) -> klsn:binstr().
new(User, Info) ->
    Passwd = random_pw(),
    Bcrypt = bcrypte:hash(Passwd, 10),
    klsn_db:upsert(?MODULE, User, fun
        ({value, _}) ->
            erlang:error(exists);
        (none) ->
            Info#{
                bcrypt => Bcrypt
              , user => User
            }
    end),
    Passwd.

-spec update_pw(user_id(), klsn:binstr(), UpdatedBy::user_id()) -> ok.
update_pw(User, Passwd, UpdatedBy) ->
    Bcrypt = bcrypte:hash(Passwd, 10),
    klsn_db:update(?MODULE, User, fun
        (Doc) ->
            Doc#{
                <<"bcrypt">> => Bcrypt
              , <<"passwd_last_updated_by">> => UpdatedBy
              , <<"passwd_last_updated_at">> => klsn_db:time_now()
            }
    end),
    ok.

random_pw() ->
    random_pw([], []).

random_pw([], Acc) ->
    random_pw(binary_to_list(crypto:strong_rand_bytes(16)), Acc);
random_pw(_, Acc) when length(Acc) =:= 12 ->
    list_to_binary(Acc);
random_pw([H|T], Acc=[]) when $A =< H, H =< $Z ->
    random_pw(T, [H|Acc]);
random_pw([H|T], Acc=[]) when $a =< H, H =< $z ->
    random_pw(T, [H|Acc]);
random_pw([H|T], Acc) when length(Acc) =:= 11, $A =< H, H =< $Z ->
    random_pw(T, [H|Acc]);
random_pw([H|T], Acc) when length(Acc) =:= 11, $a =< H, H =< $z ->
    random_pw(T, [H|Acc]);
random_pw([H|T], Acc) when 0 < length(Acc), length(Acc) < 11, 33 =< H, H =< 126 ->
    random_pw(T, [H|Acc]);
random_pw([_|T], Acc) ->
    random_pw(T, Acc).


credentials_from_header(AuthorizationHeader) ->
    case binary:split(AuthorizationHeader, <<$ >>) of
        [<<"Basic">>, EncodedCredentials] ->
            decoded_credentials(EncodedCredentials);
        _ ->
            {undefined, undefined}
    end.

decoded_credentials(EncodedCredentials) ->
    DecodedCredentials = base64:decode(EncodedCredentials),
    case binary:split(DecodedCredentials, <<$:>>) of
        [Username, Password] ->
            {Username, Password};
        _ ->
            {undefined, undefined}
    end.

