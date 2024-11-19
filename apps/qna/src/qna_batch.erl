-module(qna_batch).

-export([
        couchdb_view_map_js/0
      , start_link/0
    ]).


start_link() ->
    {ok, spawn_link(fun start_loop/0)}.

start_loop() ->
    timer:sleep(60*1000),
    loop().

loop() ->
    case list() of
        [] ->
            timer:sleep(60*1000);
        List ->
            lists:foreach(fun
                ({embed, Id}) ->
                    timer:sleep(1000),
                    qna:embed(Id);
                ({search, Id}) ->
                    % no access to openai: no need to sleep
                    qna:search(Id);
                ({ai_answer, Id}) ->
                    timer:sleep(1000),
                    qna:ai_answer(Id)
            end, List)
    end,
    loop().

list() ->
     #{<<"rows">> := Rows} =  klsn_db:get(
         qna, {raw, <<"_design/qna_batch/_view/waiting_for">>}),
     lists:map(fun
         (#{<<"id">>:=Id,<<"value">>:=<<"embed">>}) ->
             {embed, Id};
         (#{<<"id">>:=Id,<<"value">>:=<<"search">>}) ->
             {search, Id};
         (#{<<"id">>:=Id,<<"value">>:=<<"ai_answer">>}) ->
             {ai_answer, Id}
     end, Rows).
     

couchdb_view_map_js() ->
    <<"
        function (doc) {
            var state = doc.state ? doc.state : 'other';
            if (state == 'error') return;
            var waiting_for = doc.waiting_for ? doc.waiting_for : {};
            var embed = waiting_for.embed ? waiting_for.embed : false;
            var search = waiting_for.search ? waiting_for.search : false;
            var ai_answer = waiting_for.ai_answer ? waiting_for.ai_answer : false;
            if (embed)
                emit(doc.U, 'embed');
            else if (!embed && search)
                emit(doc.U, 'search');
            else if (!embed && !search && ai_answer)
                emit(doc.U, 'ai_answer');
        }
    ">>.

