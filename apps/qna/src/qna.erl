-module(qna).

-export([
        lookup/1
      , upsert/1
      , user_upsert/2
      , embed/1
      , search/1
    ]).

% debug export
-export([
        parse_metadata/1
    ]).

-type id() :: klsn:binstr().

-type qna() :: #{
        embe_id => embe:id()
      , embe_metadata => #{
            available => boolean()
          , deleted => boolean()
          , product_name => klsn:binstr()
          , product_version => klsn:binstr()
          , sheat_id => klsn:binstr()
          , no => klsn:binstr()
          , ordered_id => non_neg_integer()
          , input => klsn:binstr()
          , titles => [klsn:binstr()]
          , question => klsn:binstr()
          , notes => [klsn:binstr()]
          , qna_id => id()
        }
      , answer => klsn:binstr()
      , answer_sup => [klsn:binstr()]
      , state => init | embedded | searched | answered | error
      , last_exec => #{
            type => embed
          , at => klsn:binstr()
        }
      , waiting_for => #{
            embed => boolean()
          , search => boolean()
          , ai_answer => boolean()
        }
      , last_search_result => [id()]
      , qna_version => 1
      , log => [
            #{
                type => create
              , time => klsn:binstr()
              , user => qna_user:user_id()
              , payload => map()
            }
          | #{
                type => update
              , time => klsn:binstr()
              , user => qna_user:user_id()
              , payload => map()
            }
          | #{
                type => embed
              , time => klsn:binstr()
            }
          | #{
                type => search
              , time => klsn:binstr()
              , search_result => [#{qna_id => id(), score => float()}]
            }
        ]
    }.

-spec normalize(qna()) -> qna().
normalize(Qna0) ->
    jsone:decode(jsone:encode(Qna0)).

-spec lookup(id()) -> klsn:maybe(qna()).
lookup(QnaId) ->
    klsn_db:lookup(?MODULE, QnaId).

-spec user_upsert(qna(), qna_user:user()) -> qna().
user_upsert(Qna0, #{<<"user">>:=UserId}) ->
    Qna10 = normalize(Qna0),
    Qna20 = maps:filter(fun
        (<<"embe_metadata">>, Map) when is_map(Map) -> true;
        (<<"answer">>, Bin) when is_binary(Bin) -> true;
        (<<"answer_sup">>, List) when is_list(List) -> true;
        (<<"waiting_for">>, Map) when is_map(Map) -> true;
        (<<"_id">>, Bin) when is_binary(Bin) -> true;
        (<<"_rev">>, Bin) when is_binary(Bin) -> true;
        (_, _) -> false
    end, Qna10),
    Qna30 = maps:map(fun
        (<<"embe_metadata">>, Map) ->
            maps:filter(fun
                (<<"available">>, Bool) when is_boolean(Bool) -> true;
                (<<"deleted">>, Bool) when is_boolean(Bool) -> true;
                (<<"product_name">>, Bin) when is_binary(Bin) -> true;
                (<<"product_version">>, Bin) when is_binary(Bin) -> true;
                (<<"sheat_id">>, Bin) when is_binary(Bin) -> true;
                (<<"no">>, Bin) when is_binary(Bin) -> true;
                (<<"ordered_id">>, Int) when is_integer(Int), Int >= 0 -> true;
                (<<"titles">>, List) when is_list(List) -> true;
                (<<"question">>, Bin) when is_binary(Bin) -> true;
                (<<"notes">>, List) when is_list(List) -> true;
                (_, _) -> false
            end, Map);
        (<<"waiting_for">>, Map) ->
            maps:filter(fun
                (<<"embed">>, Bool) when is_boolean(Bool) -> true;
                (<<"search">>, Bool) when is_boolean(Bool) -> true;
                (<<"ai_answer">>, Bool) when is_boolean(Bool) -> true;
                (_, _) -> false
            end, Map);
        (_Key, Val) ->
            Val
    end, Qna20),
    #{<<"_id">>:=Id} = upsert(Qna30),
    BaseLog = #{
        time => klsn_db:time_now()
      , user => UserId
      , payload => Qna30
    },
    Log = case Qna10 of
        #{<<"_id">>:=_, <<"_rev">>:=_} ->
            BaseLog#{
                type => update
            };
        _ ->
            BaseLog#{
                type => create
            }
    end,
    klsn_map:update(?MODULE, Id, fun(Doc) ->
        Logs = case Doc of
            #{<<"logs">>:=Logs0} -> Logs0;
            _ -> []
        end,
        Doc#{<<"logs">> => [Log|Logs]}
    end).

-spec upsert(qna()) -> conflict | qna().
upsert(Qna0) ->
    Qna10 = normalize(Qna0),
    QnaId = case Qna10 of
        #{<<"_id">>:=QnaId0} ->
            QnaId0;
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
    try klsn_db:upsert(?MODULE, QnaId, fun
        (none) ->
            Qna;
        ({value, Doc}) ->
            case {Doc, Qna} of
                {#{<<"_rev">>:=DocRev},#{<<"_rev">>:=QnaRev}} when DocRev =/= QnaRev ->
                    erlang:throw({?MODULE, conflict});
                _ ->
                    ok
            end,
            merge_qna(Doc, Qna)
    end) catch
        throw:{?MODULE, conflict} ->
            conflict
    end.

-spec embed(id()) -> ok.
embed(QnaId) ->
    LastExec = #{ type => embed, at => klsn_db:time_now() },
    Qna = klsn_db:update(?MODULE, QnaId, fun(Qna0) ->
        case parse_metadata(maps:get(<<"embe_metadata">>, Qna0)) of
            none ->
                Qna0#{
                    <<"state">> => <<"error">>
                  , <<"last_exec">> => LastExec
                };
            {value, NewMetaData} ->
                Qna0#{
                    <<"embe_metadata">> => NewMetaData
                  , <<"state">> => <<"error">>
                  , <<"last_exec">> => LastExec
                }
        end
    end),
    EmbeId = case Qna of
        #{<<"embe_id">> := EmbeId0} ->
            embe:update(EmbeId0, fun(Doc) ->
                maps:merge(Doc, maps:get(<<"embe_metadata">>, Qna))
            end, new_embe()),
            EmbeId0;
        _ ->
            embe:add(maps:get(<<"embe_metadata">>, Qna), new_embe())
    end,
    klsn_db:update(?MODULE, QnaId, fun(Doc) ->
        Logs = case Doc of
            #{<<"logs">> := Logs0} -> Logs0;
            _ -> []
        end,
        klsn_map:upsert([<<"waiting_for">>, <<"embed">>], false, Doc#{
            <<"state">> => <<"embedded">>
          , <<"embe_id">> => EmbeId
          , <<"logs">> => [ #{
                type => embed
              , time => klsn_db:time_now()
            } | Logs]
        })
    end),
    ok.

-spec search(id()) -> ok.
search(QnaId) ->
    LastExec = #{ type => search, at => klsn_db:time_now() },
    Qna = klsn_db:update(?MODULE, QnaId, fun(Doc) ->
        Doc#{
            <<"state">> => <<"error">>
          , <<"last_exec">> => LastExec
        }
    end),
    EmbeId = maps:get(<<"embe_id">>, Qna),
    SearchResRaw = embe:search(EmbeId, #{
    }, new_embe()),
    SearchResQnaIds = lists:map(fun
        (#{<<"metadata">>:=#{<<"qna_id">>:=SearchQnaId}}) ->
            SearchQnaId
    end, SearchResRaw),
    SearchResLog = lists:map(fun
        (#{<<"metadata">>:=#{<<"qna_id">>:=SearchQnaId}, <<"_score">>:=Score}) ->
            #{ qna_id => SearchQnaId, score => Score }
    end, SearchResRaw),
    klsn_db:update(?MODULE, QnaId, fun(Doc) ->
        Logs = case Doc of
            #{<<"logs">> := Logs0} -> Logs0;
            _ -> []
        end,
        klsn_map:upsert([<<"waiting_for">>, <<"search">>], false, Doc#{
            <<"state">> => <<"searched">>
          , <<"last_search_result">> => SearchResQnaIds
          , <<"logs">> => [ #{
                type => search
              , time => klsn_db:time_now()
              , search_result => SearchResLog
            } | Logs]
        })
    end),
    ok.


% none if unchanged, {value, _} if input changed
-spec parse_metadata(embe:metadata()) -> klsn:maybe(embe:metadata()).
parse_metadata(MetaData0) ->
    MetaData = normalize(MetaData0),
    #{
        <<"input">> := InitInput
      , <<"titles">> := Titles
      , <<"question">> := Question
      , <<"notes">> := Notes
    } = maps:merge(#{
            <<"input">> => <<>>
          , <<"titles">> => []
          , <<"notes">> => []
        }, MetaData),
    TitleMD = titles_to_md(Titles),
    NoteMD = notes_to_md(Notes),
    Input = <<TitleMD/binary, Question/binary, NoteMD/binary>>,
    case Input of
        InitInput ->
            none;
        _ ->
            {value, MetaData#{ <<"input">> => Input } }
    end.

-spec titles_to_md([klsn:binstr()]) -> klsn:binstr().
titles_to_md([]) ->
    <<>>;
titles_to_md(Titles) ->
    Length = length(Titles),
    Seq = lists:seq(2, Length+1),
    iolist_to_binary(lists:map(fun({N, Title0}) ->
        Title10 = binary:replace(Title0, <<"\r">>, <<>>, [global]),
        Title20 = binary:replace(Title10, <<"\n">>, <<" ">>, [global]),
        Hash = lists:duplicate(N, $#),
        [Hash, <<" ">>, Title20, <<"\n\n">>]
    end, lists:zip(Seq, Titles))).

-spec notes_to_md([klsn:binstr()]) -> klsn:binstr().
notes_to_md([]) ->
    <<>>;
notes_to_md(Notes) ->
    iolist_to_binary(lists:map(fun(Note0) ->
        Note10 = binary:replace(Note0, <<"\r">>, <<>>, [global]),
        Note20 = binary:replace(Note10, <<"\n">>, <<"\n> ">>, [global]),
        <<"\n\n> [!NOTE]\n> ", Note20/binary>>
    end, Notes)).

-spec new_embe() -> embe:embeddings().
new_embe() ->
    Embe = embe:new(<<"qna_ver1">>),
    Embe#{
        embeddings_function => fun qna_ai:text_to_vector/1
    }.

-spec merge_qna(qna(), qna()) -> qna().
merge_qna(A0, B0) ->
    A = normalize(A0),
    B = normalize(B0),
    maps:map(fun(Key, Val) ->
        case maps:find(Key, A) of
            {ok, AVal} when is_map(AVal) ->
                maps:merge(AVal, Val);
            _ ->
                Val
        end
    end, maps:merge(A, B)).

