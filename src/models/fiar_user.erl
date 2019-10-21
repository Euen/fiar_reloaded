%% @doc user model
-module (fiar_user).
-author('euenlopez@gmail.com').

-behaviour(sumo_doc).

-opaque id()       :: integer().
-opaque username() :: binary().
-opaque password() :: binary().
-opaque t()        ::
  #{
    id => id(),
    username => username(),
    password => password(),
    created_at => calendar:datetime(),
    updated_at => calendar:datetime()
  }.

-export_type([t/0, id/0, username/0, password/0]).

%%% Behaviour callbacks.
-export([sumo_schema/0, sumo_wakeup/1, sumo_sleep/1]).

%%% Public API
-export([create/1, get/2, id_to_bin/1, to_view/1]).

%%====================================================================
%% Sumo Callbacks
%%====================================================================

-spec sumo_schema() -> sumo_internal:schema().
sumo_schema() ->
  sumo:new_schema(?MODULE, [
    sumo:new_field(id,         integer,  [id, not_null, auto_increment]),
    sumo:new_field(username,   string,   [{length, 255}, not_null, unique]),
    sumo:new_field(password,   string,   [{length, 255}, not_null]),
    sumo:new_field(created_at, datetime, [not_null]),
    sumo:new_field(updated_at, datetime, [not_null])
  ]).

-spec sumo_sleep(sumo:user_doc()) -> sumo:model().
sumo_sleep(User) ->
  User#{updated_at => calendar:universal_time()}.

-spec sumo_wakeup(sumo:model()) -> sumo:user_doc().
sumo_wakeup(User) ->
  User.

%%====================================================================
%% API
%%====================================================================

-spec create(map()) -> t() | {error, term()}.
create(Params) ->
  ok = validate_params(Params),
  Schema = new(Params),
  fiar_user_repo:create(Schema).

-spec to_view(t()) -> map().
to_view(#{username := Username}) ->
  #{username => Username}.

%%====================================================================
%% Getters and Setters
%%====================================================================

-spec get(atom(), t()) -> term().
get(Field, User) ->
  maps:get(Field, User).

%%====================================================================
%% Helpers
%%====================================================================

-spec new(map()) -> map().
new(Params) ->
  #{
    username   => maps:get(<<"username">>, Params),
    password   => maps:get(<<"password">>, Params),
    created_at => calendar:universal_time()
  }.

-spec id_to_bin(id()) -> binary().
id_to_bin(Id) ->
  integer_to_binary(Id).

validate_params(Params) ->
  Validations = [
    {required, [<<"username">>, <<"password">>]},
    {format, <<"username">>, fiar_validator:regex(word)},
    {format, <<"password">>, fiar_validator:regex(password)}
  ],
  fiar_validator:run(Params, Validations).