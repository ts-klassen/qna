-module(qna_ai).

-export([
        new/1
      , ask/2
      , text_to_vector/1
      , text_to_vector/2
      , fill_out/1
      , calc_cost_js/0
      , cost/1
    ]).

-type text_to_vector_opts() :: #{
        user_id => qna_user:id()
      , qna_id => qna:id()
    }.

-type new_opts() :: #{
        user_id => qna_user:id()
      , qna_id => qna:id()
    }.


-spec fill_out(
        #{
            this := qna:qna()
          , search_result := [qna:qna()]
        }
    ) -> #{log:=[
            #{
                type := ai_answering
              , time := klsn:binstr()
              , q := klsn:binstr()
              , a := klsn:binstr()
            }
          | #{
                type := ai_answer
              , time := klsn:binstr()
              , is_answerable := boolean()
              , answer => klsn:binstr()
              , answer_sup => [klsn:binstr()]
            }
        ]
      , answer := klsn:binstr()
      , answer_sup := [klsn:binstr()]
      , state := qna:state()
    }.
fill_out(#{search_result := LastQna, this := Qna}) ->
    OnUnanswerable = fun(ChildChat) ->
        throw({?MODULE, unanswerable, ChildChat})
    end,
    try
        fill_out_(Qna, LastQna, OnUnanswerable)
    of {Answer, AnswerSup, #{qna_ai_answering_logs:=ChildLog}} ->
        #{
            log => [#{
                type => ai_answer
              , time => klsn_db:time_now()
              , is_answerable => true
              , answer => Answer
              , answer_sup => AnswerSup
            } | ChildLog]
          , answer => Answer
          , answer_sup => AnswerSup
          , state => ai_answered
        }
    catch throw:{?MODULE, unanswerable, #{qna_ai_answering_logs:=ChildLog}} ->
        #{
            log => [#{
                type => ai_answer
              , time => klsn_db:time_now()
              , is_answerable => false
              , answer => <<>>
              , answer_sup => []
            } | ChildLog]
          , answer => <<>>
          , answer_sup => []
          , state => ai_unanswerable
        }
    end.


fill_out_(_Qna, [], OnUnanswerable) ->
    OnUnanswerable(new(#{}));
fill_out_(Qna, SearchedQna, OnUnanswerable) ->
    Chat0 = chat_gpte:system(
        system_msg_from_searched_qna(SearchedQna)
      , new(#{ qna_id => maps:get(<<"_id">>, Qna) })
    ),
    Chat10 = chat_gpte:system(system_msg_from_unanswered_qna(Qna), Chat0),
    {_, Chat20} = ask(<<"過去回答に、今回の質問に関連する情報はありますか？\n\n補足: 「対象外」という過去回答は、有益な情報です。過去の同様の質問に「対象外」と回答したことを根拠に、今回の質問にも「対象外」と回答できます。（逆に、過去回答で「対象外」が見つからない場合は、明確な根拠がない限り、自己判断で「対象外」とすることは許可されていません。）"/utf8>>, Chat10),
    case has_related_questions(Chat20) of
        true -> ok;
        false -> OnUnanswerable(Chat20)
    end,
    {_, Chat30} = ask(<<"回答根拠にできそうな過去回答をピックアップしてください。"/utf8>>, Chat20),
    {_, Chat40} = ask(<<"ピックアップした回答根拠に基づき、回答を作成してください。\n回答は、1つの主文と、0個以上の補足で構成されます。通常、主文は『はい』『いいえ』『該当なし』『対象外』など短く端的な文章で、詳細は補足に記載します。『◯』や『✕』などの記号で回答を求められている場合は、記号のみを主文に記載してください。"/utf8>>, Chat30),
    OnUnanswerable(Chat40),
    {<<"テスト回答"/utf8>>, [<<"テスト補足1"/utf8>>, <<"テスト補足1"/utf8>>], Chat20}.


-spec has_related_questions(chat_gpte:chat()) -> boolean().
has_related_questions(Chat0) ->
    Schema = #{
        name => has_related_questions
      , schema => #{
            type => object
          , properties => #{
                has_related_questions => #{ type => boolean }
            }
        }
    },
    Chat10 = chat_gpte:schema(Schema, Chat0),
    {JSON, _} = ask(<<"Reply to me in JSON format.">>, Chat10),
    #{<<"has_related_questions">>:=Res} = jsone:decode(JSON),
    Res.

system_msg_from_searched_qna(Qnas) ->
    Data = lists:map(fun
        (#{<<"embe_metadata">>:=#{<<"titles">>:=Titles, <<"question">>:=Question, <<"notes">>:=Notes}, <<"answer">>:=Answer, <<"answer_sup">>:=AnswerSup}) ->
            {[
                {titles, Titles}
              , {question, Question}
              , {notes, Notes}
              , {answer, Answer}
              , {answer_sup, AnswerSup}
            ]}
    end, Qnas),
    JSON = jsone:encode(Data, [native_utf8, {indent, 2}, {space, 1}]),
    iolist_to_binary([
        <<"以下の過去回答に基づいて回答してください。過去回答からは判断できない場合は、分からないことを user に伝えて、適切にエスカレーションしてください。\n\n"/utf8>>
      , JSON
    ]).


system_msg_from_unanswered_qna(#{<<"embe_metadata">>:=#{<<"titles">>:=Titles, <<"question">>:=Question, <<"notes">>:=Notes}}) ->
    Data = {[
        {titles, Titles}
      , {question, Question}
      , {notes, Notes}
    ]},
    JSON = jsone:encode(Data, [native_utf8, {indent, 2}, {space, 1}]),
    iolist_to_binary([
        <<"user の指示に従い、次の質問への回答を考えてください。\n\n"/utf8>>
      , JSON
    ]).

-spec new(new_opts()) -> chat_gpte:chat().
new(Opts) ->
    Model = <<"gpt-4o-mini">>,
    Chat0 = chat_gpte:new(),
    Chat10 = chat_gpte:model(Model, Chat0),
    Chat20 = chat_gpte:temperature(1.0, Chat10),
    Chat20#{  qna_ai_opts => Opts, qna_ai_answering_logs => [] }.

-spec ask(klsn:binstr(), chat_gpte:chat()) -> {klsn:binstr(), chat_gpte:chat()}.
ask(Text, Chat0) ->
    {Res, Chat10} = chat_gpte:ask(Text, Chat0),
    Chat20 = Chat10#{
        qna_ai_answering_logs => [
            #{
                type => ai_answering
              , time => klsn_db:time_now()
              , q => Text
              , a => Res
            }
          | klsn_map:get([qna_ai_answering_logs], Chat10, [])
        ]
    },
    log_chat_gpte(Chat20),
    {Res, Chat20}.

-spec log_chat_gpte(chat_gpte:chat()) -> ok.
log_chat_gpte(Chat) ->
    Usage = chat_gpte:get_last_usage(Chat),
    Messages = lists:map(fun
        ({Type, Value}) when is_binary(Value), is_atom(Type) ->
            #{type => Type, value => Value};
        (Other) ->
            Value = iolist_to_binary(io_lib:format("~p", [Other])),
            #{type => unknown, value => Value}
    end, chat_gpte:messages(Chat)),
    {_, _} = klsn_db:create_doc(?MODULE, #{
        messages => Messages
      , model => klsn_maybe:get_value(chat_gpte:model(Chat))
      , usage => Usage
      , type => chat
      , version => 1
      , qna_ai_opts => klsn_map:get([qna_ai_opts], Chat, #{})
    }),
    ok.

-spec text_to_vector(klsn:binstr()) -> embe_vector_db:vector().
text_to_vector(Text) ->
    text_to_vector(Text, #{}).

-spec text_to_vector(
        klsn:binstr(), text_to_vector_opts()
    ) -> embe_vector_db:vector().
text_to_vector(Text, Opts) ->
    Model = <<"text-embedding-3-large">>,
    Emb0 = gpte_embeddings:new(),
    Emb10 = gpte_embeddings:model(Model, Emb0),
    Emb = gpte_embeddings:embed(Text, Emb10),
    Usage = gpte_embeddings:get_last_usage(Emb),
    {_, _} = klsn_db:create_doc(?MODULE, #{
        input => Text
      , model => Model
      , usage => Usage
      , type => t2v
      , version => 1
      , qna_ai_opts => Opts
    }),
    gpte_embeddings:get_vector(Emb).

-spec cost(model|year|month|day) -> [#{key:=klsn:binstr(), value:=float()}].
cost(model) ->
    cost(<<"1">>);
cost(year) ->
    cost(<<"2">>);
cost(month) ->
    cost(<<"3">>);
cost(day) ->
    cost(<<"4">>);
cost(Level) when is_binary(Level) ->
    Id = <<"_design/qna_ai/_view/cost?reduce=true&group_level=", Level/binary>>,
    #{<<"rows">>:=Rows} = klsn_db:get(?MODULE, {raw, Id}),
    lists:map(fun(#{<<"key">>:=Key,<<"value">>:=Value}) ->
        #{key => Key, value => Value}
    end, Rows).

calc_cost_js() ->
    <<"function (doc) {
        var key = doc.C.split('T')[0].split('-');
        key.unshift(doc.model);
        var cost = 0;
        if (doc.model == 'gpt-4o-mini') {
            cost += 0.000000150 * doc.usage.prompt_tokens;
            cost += 0.000000600 * doc.usage.completion_tokens;
        }
        if (doc.model == 'text-embedding-3-large') {
            cost += 0.000000130 * doc.usage.prompt_tokens;
        }
        emit(key, cost);
    }">>.

