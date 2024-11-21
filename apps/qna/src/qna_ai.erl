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
fill_out(#{search_result := []}) ->
    #{
        log => [#{
            type => ai_answering
          , time => klsn_db:time_now()
          , q => <<"類似する過去の回答がありません。"/utf8>>
          , a => <<"回答できませんでした。"/utf8>>
        }]
      , answer => <<>>
      , answer_sup => []
      , state => ai_unanswerable
    };
fill_out(#{search_result := LastQna, this := Qna}) ->
    OnUnanswerable = fun(ChildLog) ->
        throw({?MODULE, unanswerable, ChildLog})
    end,
    try
        fill_out_(Qna, LastQna, OnUnanswerable)
    of {Answer, AnswerSup, ChildLog} ->
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
    catch throw:{?MODULE, unanswerable, ChildLog} ->
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
    Log10 = [#{
        type => ai_answering
      , time => klsn_db:time_now()
      , q => <<"類似する過去の回答がありません。"/utf8>>
      , a => <<"回答できませんでした。"/utf8>>
    }],
    OnUnanswerable(Log10);
fill_out_(_Qna, _SearchedQna, _OnUnanswerable) ->
    Log10 = [#{
        type => ai_answering
      , time => klsn_db:time_now()
      , q => <<"類似する過去の回答があります！"/utf8>>
      , a => <<"未実装ですが回答します。"/utf8>>
    }],
    {<<"テスト回答"/utf8>>, [<<"テスト補足1"/utf8>>, <<"テスト補足1"/utf8>>], Log10}.


-spec new(new_opts()) -> chat_gpte:chat().
new(Opts) ->
    Model = <<"gpt-4o-mini">>,
    Chat0 = chat_gpte:new(),
    Chat10 = chat_gpte:model(Model, Chat0),
    Chat20 = chat_gpte:temperature(1.0, Chat10),
    Chat20#{  qna_ai_opts => Opts }.

-spec ask(klsn:binstr(), chat_gpte:chat()) -> {klsn:binstr(), chat_gpte:chat()}.
ask(Text, Chat0) ->
    {Res, Chat10} = chat_gpte:ask(Text, Chat0),
    log_chat_gpte(Chat10),
    {Res, Chat10}.

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
      , model => chat_gpte:model(Chat)
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

