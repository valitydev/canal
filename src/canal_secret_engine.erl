-module(canal_secret_engine).

-export([make_read_request/2,
         make_write_request/3,
         handle_read_request/2,
         handle_write_request/2]).

%% See `httpc:request/5`

-type httpc_request() ::
    {uri_string:uri_string(), [http_header()]} |
    {uri_string:uri_string(),
     [http_header()],
     ContentType :: uri_string:uri_string(),
     http_body()}.

-type http_body() ::
    iolist() |
    binary() |
    {fun((Accumulator :: term()) ->
             eof | {ok, iolist(), Accumulator :: term()}),
     Accumulator :: term()} |
    {chunkify,
     fun((Accumulator :: term()) ->
             eof | {ok, iolist(), Accumulator :: term()}),
     Accumulator :: term()}.

-type http_header() :: {Field :: [byte()], Value :: binary() | iolist()}.

-type params() :: #{
    engine := canal:secret_engine(),
    url := uri_string:uri_string(),
    headers := [http_header()]
}.

-type status_line() :: {uri_string:uri_string(), non_neg_integer(), string()}.

-type response() ::
    {status_line(), [http_header()], uri_string:uri_string() | binary()}.

-export_type([params/0, response/0, httpc_request/0]).

-callback make_read_request(canal:secret_key(), params()) -> httpc_request().

-callback make_write_request(
    canal:secret_key(),
    canal:secret_value(),
    params()
) -> httpc_request().

-callback handle_read_request(response()) ->
    {ok, canal:secret_value()} | {error, {non_neg_integer(), [term()]}}.

-callback handle_write_request(response()) ->
    ok | {error, {non_neg_integer(), [term()]}}.

-spec make_read_request(canal:secret_key(), params()) -> httpc_request().
make_read_request(Key, Params) ->
    Mod = get_secret_engine_module(Params),
    Mod:make_read_request(Key, Params).

-spec make_write_request(
    canal:secret_key(),
    canal:secret_value(),
    params()
) -> httpc_request().
make_write_request(Key, Value, Params) ->
    Mod = get_secret_engine_module(Params),
    Mod:make_write_request(Key, Value, Params).

-spec handle_read_request(response(), params()) ->
    {ok, canal:secret_value()} | {error, {non_neg_integer(), [term()]}}.
handle_read_request(Response, Params) ->
    Mod = get_secret_engine_module(Params),
    Mod:handle_read_request(Response).

-spec handle_write_request(response(), params()) ->
    ok | {error, {non_neg_integer(), [term()]}}.
handle_write_request(Response, Params) ->
    Mod = get_secret_engine_module(Params),
    Mod:handle_write_request(Response).

get_secret_engine_module(#{engine := kvv2}) ->
    canal_secret_engine_kvv2;
get_secret_engine_module(#{engine := kvv1}) ->
    canal_secret_engine_kvv1.
