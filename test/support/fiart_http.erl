-module(fiart_http).

-include_lib("common_test/include/ct.hrl").

-export([
  get/2,
  get/3,
  post/3,
  post/4,
  call/5,
  basic_auth_header/2,
  wrong_auth_header/0,
  wrong_auth_header/1
]).

-type http_result() :: {pos_integer(), map(), map()} |
                       {pos_integer(), map() | no_content} |
                       {error, any()}.

%%%===================================================================
%%% API
%%%===================================================================

%% @equiv get(Path, [], Config)
get(Path, Config) ->
  get(Path, [], Config).

%% @equiv call(get, Path, Headers, <<>>, Config)
get(Path, Headers, Config) ->
  call(get, Path, Headers, <<>>, Config).

%% @equiv post/4
post(Path, Body, Config) ->
  post(Path, [], Body, Config).

%% @equiv call(post, Path, Headers, Body, Config)
post(Path, Headers, Body, Config) ->
  call(post, Path, Headers, Body, Config).

-spec call(Method, Path, Headers, Body, Config) -> Res when
  Method  :: atom(),
  Path    :: binary(),
  Headers :: list(),
  Body    :: map() | binary() | list(),
  Config  :: fiar_ct:config(),
  Res     :: http_result().
call(Method, Path, Headers, Body, Config) when is_map(Body); is_list(Body) ->
  NewHeaders = [{<<"Content-Type">>, <<"application/json">>} | Headers],
  call(Method, Path, NewHeaders, jiffy:encode(Body), Config);
call(Method, Path, Headers, Body, Config) when is_binary(Body) ->
  URL = url(Path, Config),
  HttpOpts = maybe_add_cookie([{pool, false} | keyfind(http_opts, Config, [])], Config),
  % NewHeaders = maybe_add_auth_header(Headers, Config),

  case hackney:request(Method, URL, Headers, Body, HttpOpts) of
    {ok, StatusCode, RespHeaders, ClientRef} ->
      RespBody = parse_response(hackney:body(ClientRef)),
      _ = hackney:close(ClientRef),
      case keyfind(return_headers, Config) of
        true ->
          {StatusCode, maps:from_list(RespHeaders), RespBody};
        undefined ->
          {StatusCode, RespBody}
      end;
    {ok, StatusCode, RespHeaders} ->
      {StatusCode, maps:from_list(RespHeaders)};
    {error, _} = Error ->
      Error
  end.

-spec basic_auth_header(binary(), binary()) -> {binary(), binary()}.
basic_auth_header(Username, Password) ->
  Credentials = base64:encode(<<Username/binary, ":", Password/binary>>),
  {<<"Authorization">>, <<"Basic ", Credentials/binary>>}.

-spec wrong_auth_header() -> {binary(), binary()}.
wrong_auth_header() ->
  wrong_auth_header(base64:encode(<<"wrong_token">>)).

-spec wrong_auth_header(binary()) -> {binary(), binary()}.
wrong_auth_header(Credentials) ->
  {<<"Authorization">>, <<"Basic ", Credentials/binary>>}.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @private
url(Path, Config) ->
  Port = integer_to_binary(?config(port, Config)),
  <<"http://localhost:", Port/binary, Path/binary>>.

%% @private
maybe_add_cookie(HttpOpts, Config) ->
  case keyfind(auth_user, Config) of
    undefined ->
      HttpOpts;
    AuthUser ->
      case keyfind(AuthUser, Config) of
        undefined ->
          HttpOpts;
        User ->
          [{cookie, cookie(User)} | HttpOpts]
      end
  end.

-spec cookie(fiar_user:t()) -> [{binary(), binary()}].
cookie(User) ->
  Username = fiar_user:get(username, User),
  Pass = fiar_user:get(password, User),
  Token = base64:encode(<<Username/binary, ":", Pass/binary>>),
  hackney_cookie:setcookie(<<"token">>, Token, [{max_age, 10}]).

%% @private
parse_response({ok, Body}) ->
  try jiffy:decode(Body, [return_maps])
  catch _:_ -> Body
  end;
parse_response(_) ->
  #{}.

%%%===================================================================
%%% TO REMOVE
%%%===================================================================

-spec keyfind(Key, KVList) -> Val | undefined when
  Key    :: any(),
  KVList :: [{Key, Val}],
  Val    :: any().
keyfind(Key, KVList) ->
  keyfind(Key, KVList, undefined).

-spec keyfind(Key, KVList, Default) -> Val | Default when
  Key     :: any(),
  KVList  :: [{Key, Val}],
  Val     :: any(),
  Default :: any().
keyfind(Key, KVList, Default) ->
  case lists:keyfind(Key, 1, KVList) of
    {Key, Value} -> Value;
    _            -> Default
  end.