-module(fiart_http_tests).

-export([t_auth/1]).

%%%===================================================================
%%% API
%%%===================================================================

-spec t_auth(fiar_ct:config()) -> ok.
t_auth(Config) ->
  EndPoints = proplists:get_value(endpoints, Config, []),
  lists:foreach(fun({Method, Path}) ->
    t_auth(Method, Path, Config)
  end, EndPoints).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

t_auth(Method, Path, Config) ->
  try {401, #{<<"errors">> := _}} = fiart_http:call(Method, Path, [], #{}, Config)
  catch _:_ -> ct:fail("authentication check failed. EndPoint: ~p ~s", [Method, Path])
  end,

  try {401, #{<<"errors">> := _}} = fiart_http:call(Method, Path, [fiart_http:wrong_auth_header()], #{}, Config)
  catch _:_ -> ct:fail("authentication check failed. EndPoint: ~p ~s", [Method, Path])
  end,

  try {401, #{<<"errors">> := _}} = fiart_http:call(Method, Path, [fiart_http:wrong_auth_header(<<"wrong_token">>)], #{}, Config)
  catch _:_ -> ct:fail("authentication check failed. EndPoint: ~p ~s", [Method, Path])
  end,

  Credentials = base64:encode("nonexistent_user:nonexistent_pass"),
  try {401, #{<<"errors">> := _}} = fiart_http:call(Method, Path, [fiart_http:wrong_auth_header(Credentials)], #{}, Config)
  catch _:_ -> ct:fail("authentication check failed. EndPoint: ~p ~s", [Method, Path])
  end,

  try {401, #{<<"errors">> := _}} = fiart_http:call(Method, Path, [fiart_http:basic_auth_header(<<"user">>, <<"pooass">>)], #{}, Config)
  catch E:Er -> ct:fail("authorization check failed. EndPoint: ~p ~s ~p ~p", [Method, Path, E, Er])
  end.

  % {401, #{
  %   <<"error">> := <<"user does not have the necessary credentials">>
  % }} = fiart_http:get(?PATH, [], Config),

  % {401, #{
  %   <<"error">> := <<"user does not have the necessary credentials">>
  % }} = fiart_http:get(?PATH, [fiart_http:wrong_auth_header()], Config),

  % {401, #{
  %   <<"error">> := <<"user does not have the necessary credentials">>
  % }} = fiart_http:get(?PATH, [fiart_http:wrong_auth_header(<<"wrong_token">>)], Config),

  % Credentials = base64:encode("nonexistent_user:nonexistent_pass"),
  % {401, #{
  %   <<"error">> := <<"user does not have the necessary credentials">>
  % }} = fiart_http:get(?PATH, [fiart_http:wrong_auth_header(Credentials)], Config).
