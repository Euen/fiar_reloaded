-module(fiart_current_user_handler_SUITE).
-author('euenlopez@gmail.com').

% -include_lib("/usr/lib/erlang/lib/common_test-1.15.3/include/ct.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("mixer/include/mixer.hrl").
-mixin([{fiart_ct, [init_per_suite/1, end_per_suite/1]}, fiart_http_tests]).

%% CT
-export([
  all/0,
  init_per_testcase/2,
  end_per_testcase/2
]).

%% Test Cases
-export([
  success/1,
  success_with_cookie/1
]).

-define(PATH, <<"/me">>).

%%%===================================================================
%%% CT Callbacks
%%%===================================================================

all() ->
  [
    success,
    success_with_cookie,
    t_auth
  ].

init_per_testcase(t_auth, Config) ->
  [{endpoints, [{get, ?PATH}]} | Config];
init_per_testcase(_, Config) ->
  [
    {auth_user, user},
    {user, fiart_db_utils:create_user()} |
    Config
  ].

end_per_testcase(_, Config) ->
  _ = fiar_user_repo:delete_all(),
  Config.

%%%===================================================================
%%% Test Cases
%%%===================================================================

success(Config) ->
  User     = ?config(user, Config),
  Username = fiar_user:get(username, User),
  Pass     = fiar_user:get(password, User),
  Headers  = [fiart_http:basic_auth_header(Username, Pass)],

  {200, #{
    <<"user">> := #{<<"username">> := Username},
    <<"current_matches">> := []
  }} = fiart_http:get(?PATH, Headers, Config),

  [Match1, Match2] = fiart_db_utils:create_matches(2, User),

  {200, #{
    <<"user">> := #{<<"username">> := Username},
    <<"current_matches">> := Matches
  }} = fiart_http:get(?PATH, Headers, Config),

  true = lists:member(Match1, Matches),
  true = lists:member(Match2, Matches).

success_with_cookie(Config) ->
  User     = ?config(user, Config),
  Username = fiar_user:get(username, User),

  {200, #{
    <<"user">> := #{<<"username">> := Username},
    <<"current_matches">> := []
  }} = fiart_http:get(?PATH, Config),

  [Match1, Match2] = fiart_db_utils:create_matches(2, User),

  {200, #{
    <<"user">> := #{<<"username">> := Username},
    <<"current_matches">> := Matches
  }} = fiart_http:get(?PATH, Config),

  true = lists:member(Match1, Matches),
  true = lists:member(Match2, Matches).
