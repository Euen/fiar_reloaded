-module(fiart_ct).

% -include_lib("/usr/lib/erlang/lib/common_test-1.15.3/include/ct.hrl").
-include_lib("common_test/include/ct.hrl").

-export([
  init_per_suite/1,
  end_per_suite/1
]).

-type config() :: proplists:proplist().

-export_type([config/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
  ok = fiar:start(),
  {ok, _} = application:ensure_all_started(hackney),
  % Port = application:get_env(webserver, port, 8080),
  {ok, #{port := Port}} = application:get_env(fiar, webserver),

  [{port, Port} | Config].

-spec end_per_suite(config()) -> ok.
end_per_suite(_Config) ->
  _ = fiar_user_repo:delete_all(),
  fiar:stop().