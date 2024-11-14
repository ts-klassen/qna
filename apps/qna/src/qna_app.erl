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
            {"/health", qna_health_handler, #{}}
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
