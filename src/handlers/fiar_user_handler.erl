-module(fiar_user_handler).

-behaviour(trails_handler).

-include_lib("mixer/include/mixer.hrl").
-mixin([{fiar_web_server, except, [allowed_methods/2, is_authorized/2]}]).

-import(fiar_web, [handle_ex/3]).

%% Callbacks
-export([trails/0, allowed_methods/2]).

%% Method handlers
-export([
  handle_post/2
]).

%%%===================================================================
%%% Trails callbacks
%%%===================================================================

%% @hidden
trails() ->
  Metadata = #{
    get => #{
      tags => ["User"],
      description => "Create a new User",
      produces => ["application/json"],
      parameters => [
        fiar_swagger:basic_auth(),
        fiar_swagger:body(user_req, [username, pass])
      ],
      responses => fiar_swagger:responses([{200, user}, 422])
    }
  },

  [trails:trail("/users", ?MODULE, [], Metadata)].

%%%===================================================================
%%% Cowboy callbacks
%%%===================================================================

%% @hidden
allowed_methods(Req, State) ->
  {[<<"POST">>], Req, State}.

%%%===================================================================
%%% Method handlers
%%%===================================================================

handle_post(Req0, State0) ->
  handle_ex(fun(Req, #{body_params := Params} = State) ->
    User = fiar_user:create(Params),
    fiar_web:response(post, fiar_user:to_view(User), Req, State)
  end, Req0, State0).
