-module(qna_ai).

-export([
        new/1
      , ask/2
      , text_to_vector/2
      , calc_cost_js/0
      , cost/1
    ]).

-type text_to_vector_opts() :: #{
        user => qna_user:user() | qna_user:id()
    }.

-type new_opts() :: #{
        user => qna_user:user() | qna_user:id()
    }.

-opaque obj() :: #{
        chat => chat_gpte:chat()
      , info => #{}
    }.

-spec new(new_opts()) -> obj().
new(Opts) ->
    Model = <<"gpt-4o-mini">>,
    Chat0 = chat_gpte:new(),
    Chat10 = chat_gpte:model(Model, Chat0),
    Chat20 = chat_gpte:temperature(1.0, Chat10),
    Info = case Opts of
        #{ user := #{<<"user">>:=User} } ->
            #{user => User};
        #{ user := User } when is_binary(User) ->
            #{user => User};
        _ ->
            #{}
    end,
    #{
        chat => Chat20
      , info => Info#{ model => Model }
    }.

-spec ask(klsn:binstr(), obj()) -> {klsn:binstr(), obj()}.
ask(Text, #{chat := Chat0, info := Info}) ->
    {Res, Chat10} = chat_gpte:ask(Text, Chat0),
    Usage = chat_gpte:get_last_usage(Chat10),
    {_, _} = klsn_db:create_doc(?MODULE, Info#{
        input => Text
      , output => Res
      , usage => Usage
      , type => t2t
      , version => 1
    }),
    {Res, #{ chat => Chat10, info => Info}}.

-spec text_to_vector(
        klsn:binstr(), text_to_vector_opts()
    ) -> embe_vector_db:vector().
text_to_vector(Text, Opts) ->
    Model = <<"text-embedding-3-large">>,
    Emb0 = gpte_embeddings:new(),
    Emb10 = gpte_embeddings:model(Model, Emb0),
    Emb = gpte_embeddings:embed(Text, Emb10),
    Usage = gpte_embeddings:get_last_usage(Emb),
    Info = case Opts of
        #{ user := #{<<"user">>:=User} } ->
            #{user => User};
        #{ user := User } when is_binary(User) ->
            #{user => User};
        _ ->
            #{}
    end,
    {_, _} = klsn_db:create_doc(?MODULE, Info#{
        input => Text
      , model => Model
      , usage => Usage
      , type => t2v
      , version => 1
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

