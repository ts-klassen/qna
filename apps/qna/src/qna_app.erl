%%%-------------------------------------------------------------------
%% @doc qna public API
%% @end
%%%-------------------------------------------------------------------

-module(qna_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/qna/health", qna_health_handler, #{}}
          , {"/qna/api/v1", qna_admin_handler, #{}}
          , {"/qna/api/v1/:arg1", qna_admin_handler, #{}}
          , {"/qna/api/v1/:arg1/:arg2", qna_admin_handler, #{}}
          , {"/qna/api/v1/:arg1/:arg2/:arg3", qna_admin_handler, #{}}
          , {"/qna/api/v2", qna_api_handler, #{}}
          , {"/qna/api/v2/:arg1", qna_api_handler, #{}}
          , {"/qna/api/v2/:arg1/:arg2", qna_api_handler, #{}}
          , {"/qna/api/v2/:arg1/:arg2/:arg3", qna_api_handler, #{}}
          , {"/qna/static/[...]", cowboy_static, {priv_dir, qna, "static"}}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(qna_http_listener,
        [{port, 9443}, inet], % add 'inet6' for ipv6
        #{
            env => #{dispatch => Dispatch}
          , middlewares => [
                qna_main_middleware
              , cowboy_router
              , cowboy_handler
            ]
        }
    ),
    qna_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
