-module(qna_master).

-export([
        get/1
      , set/3
    ]).

-spec get(klsn:binstr()) -> #{rev => klsn:binstr(), payload := #{}}.
get(Id) ->
    case klsn_db:lookup(?MODULE, Id) of
        {value, #{<<"_rev">>:=Rev, <<"payload">>:=Payload}} ->
            Base = #{ Id => [] },
            #{rev => Rev, payload => maps:merge(Base, Payload)};
        none ->
            #{ payload => #{ Id => [] } }
    end.

-spec set(klsn:binstr(), #{}, qna_user:user()) -> ok | conflict.
set(Id, Data, #{<<"user">>:=UserId}) ->
    try
        klsn_db:upsert(?MODULE, Id, fun
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
                    {#{<<"_rev">>:=Rev}, #{<<"rev">>:=Rev}} ->
                        ok;
                    _ ->
                        erlang:throw({?MODULE, conflict})
                end,
                Doc#{
                    <<"payload">> => maps:get(<<"payload">>, Data)
                  , <<"last_updated_by">> => UserId
                }
        end)
    of
        _ ->
            ok
    catch
        throw:{?MODULE, conflict} ->
            conflict
    end.

