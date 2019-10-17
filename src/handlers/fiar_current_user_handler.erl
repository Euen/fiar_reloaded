-module(fiar_current_user_handler).

-behaviour(trails_handler).

-include_lib("mixer/include/mixer.hrl").
-mixin([fiar_web_server]).

-import(fiar_web, [handle_ex/3]).

%% Trails callbacks
-export([trails/0]).

%% Method handlers
-export([
  handle_get/2
]).

%%%===================================================================
%%% Trails callbacks
%%%===================================================================

%% @hidden
trails() ->
  Metadata = #{
    get => #{
      tags => ["Me"],
      description => "Gets info about logged user",
      produces => ["application/json"],
      parameters => [fiar_swagger:basic_auth()],
      responses => fiar_swagger:responses([{200, current_user}])
    }
  },

  [trails:trail("/me", ?MODULE, [], Metadata)].

%%%===================================================================
%%% Method handlers
%%%===================================================================

handle_get(Req0, State0) ->
  handle_ex(fun(Req, #{user := User} = State) ->
    UserId         = fiar_user:get(id, User),
    CurrentMatches = fiar:current_matches(UserId),
    Response       = response(User, CurrentMatches),

    fiar_web:response(get, Response, Req, State)
  end, Req0, State0).

response(User, CurrentMatches) ->
  #{user => fiar_user:to_view(User), current_matches => fiar_match:to_view(CurrentMatches)}.