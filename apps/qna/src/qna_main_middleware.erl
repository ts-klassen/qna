-module(qna_main_middleware).
-behaviour(cowboy_middleware).

-export([execute/2]).

execute(Req0, Env) ->
    bcrypte:verify(<<"Passw0rd!">>, bcrypte:hash(<<"Passw0rd!">>)),
    IsIpOk = case Req0 of
        #{ peer := {{127,0,0,1}, _} } ->
            true;
        #{ peer := {Ip, _} } ->
            IpBin = iolist_to_binary(lists:join(<<".">>, lists:map(fun erlang:integer_to_binary/1, tuple_to_list(Ip)))),
            case qna_db:lookup(qna_ip, IpBin) of
                {value, _} ->
                    true;
                none ->
                    false
            end
    end,
    Login = case {IsIpOk, Req0} of
        {true, #{ headers := #{ <<"authorization">> := Auth }}} ->
            lookup_login(Auth);
        _ ->
            none
    end,
    Req10 = cowboy_req:set_resp_header(<<"Access-Control-Allow-Origin">>, <<"*">>, Req0),
    Req20 = Req10#{ qna_login => Login },
    Req30 = case Login of
        none ->
            unauthorized(Req20);
        _ ->
            Req20
    end,
    {ok, Req30, Env}.

lookup_login(AuthorizationHeader) ->
    {Username, Password} = credentials_from_header(AuthorizationHeader),
    case qna_db:lookup(qna_user, Username) of
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

unauthorized(Req0) ->
    cowboy_req:reply(401, #{
        <<"content-type">> => <<"text/html; charset=utf-8">>
      , <<"Www-Authenticate">> => <<"Basic realm=\"Secure Area\"">>
    }, <<"<h1>401 Unauthorized</h1>\n">>, Req0).

