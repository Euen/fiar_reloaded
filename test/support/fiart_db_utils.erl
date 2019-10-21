-module(fiart_db_utils).

-export([
  % User
  create_users/1,
  create_users/2,
  create_user/0,
  create_user/1,
  % Match
  create_matches/2]).

-spec create_users(integer()) -> fiar_user:t().
create_users(N) ->
  create_users(N, entity).

-spec create_users(integer(), atom()) -> [fiar_user:t() | any()].
create_users(N, Field) ->
  [create_user(Field) || _ <- lists:seq(1, N)].

-spec create_user() -> fiar_user:t().
create_user() ->
  create_user(entity).

-spec create_user(atom()) -> any().
create_user(Field) ->
  Rand = integer_to_binary(rand:uniform(999999)),
  Params = #{
    <<"username">> => <<"name", Rand/binary>>,
    <<"password">> => <<"password", Rand/binary>>
  },
  return_field(fiar_user, Field, fiar_user:create(Params)).

return_field(_Module, entity, Entity) -> Entity;
return_field(Module, Field, Entity) -> Module:get(Field, Entity).

create_matches(N, User) ->
  UserId1 = fiar_user:get(id, User),
  OpponentIds = create_users(N, id),
  [begin
    MatchID = fiar:start_match(UserId1, OpponentId),
    Match = fiar:fetch_match(MatchID),
    fiar_match:to_view(Match)
  end || OpponentId <- OpponentIds].