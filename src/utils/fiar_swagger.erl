-module(fiar_swagger).

%% API
-export([
  add_definitions/0,
  basic_auth/0,
  response/1,
  responses/1
]).

%% Definitions
-export([
  current_user/0,
  current_matches/0
]).

%%%===================================================================
%%% Types
%%%===================================================================

-type definition() :: atom() | {atom(), atom()}.
-type response()   :: pos_integer() | definition().
-type responses()  :: [pos_integer() | {pos_integer(), definition()}].

%%%===================================================================
%%% API
%%%===================================================================

-spec add_definitions() -> ok.
add_definitions() ->
  Defs = [
    {<<"current_user">>, current_user()},
    {<<"current_matches">>, current_matches()}
  ],

  lists:foreach(fun({Name, Props}) ->
    ok = cowboy_swagger:add_definition(Name, Props)
  end, Defs).

-spec basic_auth() -> map().
basic_auth() ->
  #{
    name        => <<"Authorization">>,
    in          => header,
    description => <<"Basic Auth Header">>,
    required    => true,
    type        => <<"string">>,
    default     => <<"Basic base64(username:password)">>
  }.

% -spec changeset_error_view() -> map().
% changeset_error_view() ->
%   #{
%     errors => #{
%       type                 => <<"object">>,
%       description          => <<"Errors Map">>,
%       additionalProperties => #{type => <<"string">>}
%     }
%   }.

% -spec error_view() -> map().
% error_view() ->
%   #{
%     error => #{
%       type        => <<"string">>,
%       description => <<"Error description">>
%     }
%   }.

-spec response(response()) -> map().
response(Def) ->
  schema(Def).

-spec responses(responses()) -> map().
responses(List) ->
  lists:foldl(fun
    ({StatusCode, Def}, Acc) ->
      Acc#{StatusCode => response(Def)};
    (StatusCode, Acc) ->
      Acc#{StatusCode => response(StatusCode)}
  end, #{}, List).

%%%===================================================================
%%% Definitions
%%%===================================================================

-spec current_user() -> map().
current_user() ->
  #{
    user => #{
      description => <<"Username">>,
      example     => <<"Poxinator">>,
      type        => <<"string">>
    },
    current_matches => #{
      type => <<"array">>,
      items => #{
        description => <<"Current matches of the player">>,
        type => <<"object">>,
        properties => ?MODULE:current_matches()
      }
    }
  }.

current_matches() ->
  #{
    player_id1 => #{
      type        => integer,
      description => <<"Id of the Player number 1">>
    },
    player_id2 => #{
      type        => integer,
      description => <<"Id of the Player number 1">>
    },
    next_chip => #{
      type => integer,
      description => <<"Next turn indicator, 1 = Player 1, 2 = Player 2">>
    },
    id => #{
      type => integer,
      description => <<"Id of the Match">>
    },
    board => #{
      type => string,
      description => <<"Board">>
    }
  }.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
schema({Def, array}) ->
  #{
    schema => #{
      type  => <<"array">>,
      items => cowboy_swagger:schema(atom_to_binary(Def, utf8))
    }
  };
schema(Def) ->
  #{
    schema => cowboy_swagger:schema(atom_to_binary(Def, utf8))
  }.