-module(qna_master).

-export([
        get/1
      , set/3
    ]).

-spec get(klsn:binstr()) -> #{rev => klsn:binstr(), payload := #{}}.
get(Id) ->
    case klsn_db:lookup(?MODULE, Id) of
        {value, #{<<"_rev">>:=Rev, <<"payload">>:=Payload}} ->
            #{rev => Rev, payload => Payload};
        none ->
            #{ payload => #{} }
    end.

-spec set(klsn:binstr(), #{}, qna_user:user()) -> ok | conflict.
set(Id, Data, #{<<"user">>:=UserId}) ->
    try
        klsn:upsert(?MODULE, Id, fun
            (none) ->
                case Data of
                    #{<<"rev">>:=_} ->
                        erlang:throw({?MODULE, conflict});
                    _ -> 
                        ok
                end,
                #{
                    payload => maps:get(<<"payload">>, Data)
                  , last_updated_by => UserId
                };
            ({value, Doc}) ->
                case {Doc, Data} of
                    {#{<<"rev">>:=A},#{<<"rev">>:=B}} when A =/= B ->
                        erlang:throw({?MODULE, conflict});
                    _ -> 
                        ok
                end,
                Doc#{
                    payload => maps:get(<<"payload">>, Data)
                  , last_updated_by => UserId
                }
        end)
    of
        _ ->
            ok
    catch
        throw:{?MODULE, conflict} ->
            conflict
    end.

