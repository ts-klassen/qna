-module(qna_admin_handler).
-behavior(cowboy_rest).

-export([init/2]).

init(Req0=#{method:=Method, qna_login:={value, Login=#{<<"is_admin">>:=true}}, bindings:=Bindings}, State) ->
    {ok, ReqBody, Req10} = cowboy_req:read_body(Req0),
    JSON = case ReqBody of
        <<>> -> #{};
        _ -> jsone:decode(ReqBody)
    end,
    Body = try
        jsone:encode(main(Method, Bindings, Login, JSON))
    catch
        Class:Error:Stack ->
            qna_oplog:error(Req10, ReqBody, Class, Error, Stack),
            jsone:encode(#{success => false, reason => server_error})
    end,
    qna_oplog:ok(Req10, ReqBody, jsone:decode(Body)),
    Req = cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/html; charset=utf-8">>
    }, Body, Req0),
    {ok, Req, State};

init(Req0, State) ->
    Req = cowboy_req:reply(403, #{
        <<"content-type">> => <<"text/html; charset=utf-8">>
    }, <<"<h1>403 Forbidden</h1>\n">>, Req0),
    {ok, Req, State}.


main(<<"GET">>, #{arg1 := <<"users">>}, _, _) ->
    #{success => true, users => qna_user:list()};


main(<<"POST">>, #{arg1 := <<"users">>}, #{<<"user">>:=User}, #{<<"id">>:=NewUserId, <<"is_admin">>:=IsAdmin}) ->
    #{success => true, passwd => qna_user:new(NewUserId, #{is_admin => IsAdmin, created_by => User})};


main(<<"POST">>, #{arg1 := <<"users">>, arg2 := <<"passwd">>}, #{<<"user">>:=User}, #{<<"id">>:=ThisUserId, <<"passwd">>:=Passwd}) ->
    qna_user:update_pw(ThisUserId, Passwd, User),
    #{success => true};


main(<<"GET">>, #{arg1 := <<"ip">>}, _, _) ->
    #{success => true, ip_list => qna_ip:list()};


main(<<"POST">>, #{arg1 := <<"ip">>}, #{<<"user">>:=User}, #{<<"ip">>:=Ip, <<"memo">>:=Memo}) ->
    qna_ip:new(Ip, #{memo => Memo, created_by => User}),
    #{success => true};


main(_, _, _, _) ->
    #{ success => false, reason => clause_error }.

