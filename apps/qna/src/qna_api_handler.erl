-module(qna_api_handler).
-behavior(cowboy_rest).

-export([init/2]).

init(Req0=#{method:=Method, qna_login:={value, Login}, bindings:=Bindings}, State) ->
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
    io:format("~p~n", [Req]),
    {ok, Req, State}.


main(<<"POST">>, #{arg1 := <<"qna">>, arg2 := <<"upsert">>}, User, Payload) ->
    case qna:user_upsert(Payload, User) of
        conflict ->
            #{success => false, reason => conflict};
        Qna ->
            #{success => true, qna => Qna}
    end;


main(<<"POST">>, #{arg1 := <<"qna">>, arg2 := <<"lookup">>}, _, #{<<"id">>:=Id}) ->
    case qna:lookup(Id) of
        none ->
            #{success => false, reason => not_found};
        {value, Qna} ->
            #{success => true, qna => Qna}
    end;


main(<<"GET">>, #{arg1 := <<"qna">>, arg2 := <<"state">>}, _, _) ->
    #{success => true, state_list => qna_state:states()};


main(<<"POST">>, #{arg1 := <<"qna">>, arg2 := <<"list">>}, _, #{<<"state">>:=State}) ->
    #{success => true, qna_list => qna_state:list(State)};


main(<<"GET">>, #{arg1 := <<"master">>, arg2 := Id}, _, _) ->
    Master = qna_master:get(Id),
    Master#{ success => true };


main(<<"POST">>, #{arg1 := <<"master">>, arg2 := Id}, User, Payload) ->
    case qna_master:set(Id, Payload, User) of
        conflict ->
            #{success => false, reason => conflict};
        ok ->
            #{success => true}
    end;


main(<<"POST">>, #{arg1 := <<"users">>, arg2 := <<"passwd">>}, #{<<"user">>:=User}, #{<<"passwd">>:=Passwd}) ->
    qna_user:update_pw(User, Passwd, User),
    #{success => true};



main(_, _, _, _) ->
    #{ success => false, reason => clause_error }.

