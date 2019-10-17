-module(fiar_web_server).

%% Cowboy
-export([
  init/2,
  rest_init/2,
  allowed_methods/2,
  resource_exists/2,
  content_types_provided/2,
  content_types_accepted/2,
  is_authorized/2
  % handle_badreq/2
]).

%%%===================================================================
%%% Cowboy Callbacks
%%%===================================================================

init(Req, _State) ->
  {cowboy_rest, Req, #{}}.

rest_init(Req, State) ->
  {ok, Req, State}.

allowed_methods(Req, State) ->
  {[<<"GET">>], Req, State}.

-spec resource_exists(Req, State) -> Res when
  Req   :: cowboy_req:req(),
  State :: any(),
  Res   :: {boolean(), cowboy_req:req(), any()}.
resource_exists(Req, State) ->
  Method = cowboy_req:method(Req),
  {Method =/= <<"POST">>, Req, State}.

content_types_provided(Req, State) ->
  {[{<<"application/json">>, handle_get}], Req, State}.

content_types_accepted(Req, State) ->
  Method = cowboy_req:method(Req),
  {Params, Req2} = parse_media_type(Req),

  Function =
    case {Method, Params} of
      % {_, undefined}   -> handle_badreq;
      {<<"POST">>, _}  -> handle_post
      % {<<"PUT">>, _}   -> handle_put;
      % {<<"PATCH">>, _} -> handle_patch
    end,

  {[{<<"application/json">>, Function}], Req2, State#{body_params => Params}}.

is_authorized(Req, State) ->
  case fiar_auth:check_auth(Req) of
    {authenticated, User, _Req1} ->
      {true, Req, #{user => User}};
    {not_authenticated, AuthHeader, Req1} ->
      Req2 = fiar_error_view:render(unauthorized, Req1),
      {{false, AuthHeader}, Req2, State}
  end.

% handle_badreq(Req, State) ->
  % Req1 = pw_error_view:render(bad_request, Req),
  % {false, Req1, State}.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @private
% unauthorized(Req, State) ->
%   Req1 = pw_error_view:render(unauthorized, Req),
%   {{false, <<"realm=\"Pegb Wallet\"">>}, Req1, State}.

%% @private
parse_media_type(Req) ->
  try
    {Type, SubType, _} = cowboy_req:parse_header(<<"content-type">>, Req),
    {ok, Data, Req2} = read_full_body(Req, 1024 * 1024 * 10), % 10MB default, TODO: make configurable?
    {decode_media_type(Data, Type, SubType), Req2}
  catch
    _:_ ->
      {undefined, Req}
  end.

%% @private
read_full_body(Req, Limit) ->
  read_full_body(Req, Limit, []).

read_full_body(_, Limit, _) when Limit =< 0 ->
  {error, body_is_too_big};

read_full_body(Req, Limit, Acc) ->
  case cowboy_req:read_body(Req) of
    {ok, Data, Req2} ->
      Body = iolist_to_binary(lists:reverse([Data | Acc])),
      {ok, Body, Req2};
    {more, Data, Req2} ->
      read_full_body(Req2, Limit - byte_size(Data), [Data | Acc])
  end.

%% @TODO: maybe add more media types
%% @private
decode_media_type(Data, <<"application">>, <<"json">>) ->
  fiar_utils:json_decode(Data).