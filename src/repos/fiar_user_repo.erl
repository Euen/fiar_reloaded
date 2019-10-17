-module(fiar_user_repo).

-export([
  create/1,
  find_by_username/1,
  find_by_id/1,
  delete/1,
  delete_all/0
]).

-type t() :: fiar_user:t().
-type username() :: fiar_user:username().
% -type password() :: fiar_user:password().

-spec create(map()) -> t() | no_return().
create(Schema) ->
  case find_by_username(fiar_user:get(username, Schema)) of
    notfound ->
      sumo:persist(fiar_user, Schema);
    User ->
      throw({conflict, fiar_user, fiar_user:get(id, User)})
  end.

% -spec get(string(), string()) -> notfound | t().
% get(Username, Pass) ->
%   case sumo:find_by(fiar_user, [{username, Username}, {pass, Pass}]) of
%     []     -> notfound;
%     [User] -> User
%   end.

-spec find_by_username(username()) -> t() | notfound.
find_by_username(Username) ->
  case sumo:find_by(fiar_user, [{username, Username}]) of
    [User] -> User;
    _      -> notfound
  end.

-spec find_by_id(integer()) -> t().
find_by_id(UserId) ->
  sumo:find_one(fiar_user, {id, UserId}).

-spec delete(pos_integer()) -> boolean().
delete(Id) ->
  sumo:delete(fiar_user, Id).

-spec delete_all() -> non_neg_integer().
delete_all() ->
  sumo:delete_all(fiar_user).
