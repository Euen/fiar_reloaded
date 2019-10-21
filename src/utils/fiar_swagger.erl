-module(fiar_swagger).

%% API
-export([
  add_definitions/0,
  body/1,
  body/2,
  basic_auth/0,
  response/1,
  responses/1
]).

%% Definitions
-export([
  current_user/0,
  current_matches/0,
  user_req/0,
  user/0
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
    {<<"current_matches">>, current_matches()},
    {<<"user_req">>, user_req()},
    {<<"user">>, user()},
    {<<"error">>, error()}
  ],

  lists:foreach(fun({Name, Props}) ->
    ok = cowboy_swagger:add_definition(Name, Props)
  end, Defs).

-spec body(atom()) -> map().
body(Def) ->
  body(Def, []).

-spec body(atom() | {atom(), array}, list()) -> map().
body({Def, array}, Required) ->
  #{
    name => <<"request body">>,
    in => body,
    description => <<"request body (as json)">>,
    required => true,
    schema => #{
      type => <<"array">>,
      items => #{
        type => <<"object">>,
        properties => ?MODULE:Def(),
        required => Required
      }
    }
  };
body(Def, Required) ->
  #{
    name => <<"request body">>,
    in => body,
    description => <<"request body (as json)">>,
    required => true,
    schema => #{
      type => <<"object">>,
      properties => ?MODULE:Def(),
      required => Required
    }
  }.

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

-spec error_view() -> map().
error_view() ->
  #{
    errors => #{
      type        => <<"array">>,
      description => <<"Errors Map">>,
      items       => schema(<<"error">>)
    }
  }.

-spec error() -> map().
error() ->
  #{
    message => #{
      type => string,
      description => <<"Error message">>
    }
  }.

-spec response(response()) -> map().
response(StatusCode) when is_integer(StatusCode), StatusCode >= 400, StatusCode < 600 ->
  #{
    description => http_error_desc(StatusCode),
    schema => #{
      type       => <<"object">>,
      properties => error_view()
    }
  };

response(204) ->
  #{description => <<"no content provided">>};

response(202) ->
  #{description => <<"Request accepted">>};

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

-spec current_matches() -> map().
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

-spec user_req() -> map().
user_req() ->
  #{
    username => #{
      type => string,
      description => <<"Username">>
    },
    pass => #{
      type => string,
      description => <<"Password">>
    }
  }.

-spec user() -> map().
user() ->
  #{
    username => #{
      type => string,
      description => <<"Username">>
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
      items => cowboy_swagger:schema(fiar_utils:to_bin(Def))
    }
  };

schema(Def) ->
  #{
    schema => cowboy_swagger:schema(fiar_utils:to_bin(Def))
  }.

% http_error_desc(400) ->
%   <<"malformed request syntax, size too large or invalid request message framing">>;
http_error_desc(401) ->
  <<"authentication failed">>;
% http_error_desc(403) ->
%   <<"authorization failed">>;
% http_error_desc(404) ->
%   <<"requested resource wasn't found">>;
http_error_desc(422) ->
  <<"request was well-formed but was unable to be followed due to semantic errors">>;
% http_error_desc(500) ->
%   <<"internal server error">>;
% http_error_desc(503) ->
%   <<"service unavailable">>;
% http_error_desc(504) ->
%   <<"Gateway timeout">>;
http_error_desc(_) ->
  <<"">>.