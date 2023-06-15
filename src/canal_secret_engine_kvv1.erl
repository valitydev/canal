-module(canal_secret_engine_kvv1).

-include("canal_internal.hrl").

-behaviour(canal_secret_engine).

-export([make_read_request/2,
         make_write_request/3,
         handle_read_request/1,
         handle_write_request/1]).

-spec make_read_request(
    canal:secret_key(),
    canal_secret_engine:params()
) -> canal_secret_engine:httpc_request().
make_read_request(Key, Params) ->
    {url(Params, ["/v1/secret/", Key]), headers(Params)}.

-spec make_write_request(
    canal:secret_key(),
    canal:secret_value(),
    canal_secret_engein:params()
) -> canal_secret_engine:httpc_request().
make_write_request(Key, Value, Params) ->
    {url(Params, ["/v1/secret/", Key]),
     headers(Params),
     ?CONTENT_TYPE_JSON,
     ?ENCODE(Value)}.

-spec handle_read_request(canal_secret_engine:response()) ->
    {ok, canal:secret_value()} | {error, {non_neg_integer(), [term()]}}.
handle_read_request({{_, StatusCode, _}, _, Reply}) ->
    case ?DECODE(Reply) of
        #{<<"data">> := Data} ->
            {ok, Data};
        #{<<"errors">> := Errors} ->
            {error, {StatusCode, Errors}}
    end.

-spec handle_write_request(canal_secret_engine:response()) ->
    ok | {error, {non_neg_integer(), [term()]}}.
handle_write_request({{_, 204, _}, _, _}) ->
    ok;
handle_write_request({{_, StatusCode, _}, _, Body}) ->
    case ?DECODE(Body) of
        #{<<"errors">> := Errors} ->
            {error, {StatusCode, Errors}};
        #{<<"warnings">> := Warnings} when is_list(Warnings) ->
            canal_utils:warning_msg(
              "write operation warnings: ~p", [Warnings]),
            ok;
        _ ->
            ok
    end.

url(#{url := BaseUrl}, Key) ->
    binary_to_list(iolist_to_binary([BaseUrl | Key])).

headers(#{headers := Headers}) ->
    Headers.
