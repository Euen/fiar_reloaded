%%%-------------------------------------------------------------------
%% @doc fiar public API
%% @end
%%%-------------------------------------------------------------------

-module(fiar).
-author('euenlopez@gmail.com').

-behaviour(application).

-type user_id()       :: fiar_user:id().
-type username()      :: fiar_user:username().
-type password()      :: fiar_user:password().
-type user()          :: fiar_user:t().
-type match_id()      :: fiar_match:id().
-type match()         :: fiar_match:t().
-type match_col_num() :: fiar_match:col_num().
-type match_reply()   :: fiar_match:reply().

%% Application callbacks
-export([
  start/0,
  start/2,
  stop/0,
	stop/1,
  start_phase/3
]).

-export([
  create_user/2,
  start_match/2,
  play/3,
  current_matches/1,
  fetch_match/1
]).

%%====================================================================
%% API
%%====================================================================

-spec start() -> ok.
start() ->
  {ok, _} = application:ensure_all_started(fiar),
  sumo:create_schema().

-spec stop() -> ok | {error, term()}.
stop() ->
  application:stop(fiar).

-spec create_user(username(), password()) -> user() | {error, term()}.
create_user(Username, Password) ->
  fiar_user:create(#{<<"username">> => Username, <<"password">> => Password}).

-spec start_match(user_id(), user_id()) -> match_id().
start_match(UserId1, UserId2) ->
  fiar_match:create(UserId1, UserId2).

-spec fetch_match(match_id()) -> match().
fetch_match(MatchId) ->
  fiar_match:fetch(MatchId).

-spec play(match_id(), match_col_num(), user_id()) -> match_reply().
play(MatchId, Column, UserId) ->
  fiar_match:play(MatchId, Column, UserId).

-spec current_matches(user_id()) -> [match()] | [].
current_matches(UserId) ->
  fiar_match:current_matches(UserId).

%%====================================================================
%% Application callbacks
%%====================================================================

-spec start(any(), any()) -> {ok, pid()} | {error, any()}.
start(_StartType, _StartArgs) ->
  sumo:create_schema(),
  fiar_sup:start_link().

-spec stop(any()) -> ok.
stop(_State) ->
  cowboy:stop_listener(?MODULE).

%% @TODO: check this spec, and why a start_phase
-spec start_phase(atom(), any(), []) -> ok.
start_phase(start_web_server, _StartType, []) ->
  Trails = trails:trails(handlers()),
  ok = trails:store(Trails),
  Routes = trails:single_host_compile(Trails),
  ok = fiar_swagger:add_definitions(),
  start_web_server(Routes).


%%====================================================================
%% Internal
%%====================================================================

%% @private
start_web_server(Routes) ->
  {ok, #{
    port      := Port,
    acceptors := Acceptors
  }} = application:get_env(fiar, webserver),
  Options = [{port, Port}, {num_acceptors, Acceptors}],

  {ok, _} = cowboy:start_clear(?MODULE, Options, #{env => #{dispatch => Routes}}),
  ok.

handlers() ->
  [
    cowboy_swagger_handler,
    fiar_current_user_handler,
    fiar_user_handler
  ].

% start_cowboy_listeners() ->
%   Dispatch = cowboy_router:compile([
%     {'_', [
%       {"/me", fiar_current_user_handler, []}
%       % {<<"/">>, cowboy_static, {file, "./priv/static/index.html"}},
%     ]}
%   ]),

%   Port = application:get_env(cowboy, http_port, 8080),
%   cowboy:start_clear(fiar_http_listener, [{port, Port}], #{env => #{dispatch => Dispatch}}).