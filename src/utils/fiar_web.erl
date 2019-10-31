-module(fiar_web).

%% API
-export([
  handle_ex/3,
  response/4
]).

%%%===================================================================
%%% API
%%%===================================================================

-spec handle_ex(Fun, Req, State) -> Res when
  Fun   :: fun((any(), any()) -> any()),
  Req   :: cowboy_req:req(),
  State :: any(),
  Res   :: any().
handle_ex(Fun, Req, State) ->
  try
    Fun(Req, State)
  catch
    Class:Error:STrace ->
      % erlang:display([Class, Error, Stacktrace]),
      ok = lager:warning("~p ~p \n\n Stacktrace: ~s \n\n   ~s\n\n Handler State:\n\n   ~s\n", [
        Class,
        Error,
        io_lib_pretty:print(STrace, [{column, 3}]),
        io_lib_pretty:print(State,  [{column, 3}])
      ]),
      handle_ex(Class, Error, STrace, Req, State)
  end.

-spec response(Method, Response, Req, State) -> Res when
  Method   :: get | post,
  Response :: map(),
  Req      :: cowboy_req:req(),
  State    :: any(),
  Res      :: {iodata() | boolean(), Req, State}.
response(get, Response, Req, State) ->
  {fiar_utils:json_encode(Response), Req, State};

response(post, Response, Req, State) ->
  RespBody = fiar_utils:json_encode(Response),
  Req1 = cowboy_req:set_resp_body(RespBody, Req),
  {true, Req1, State}.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

-spec handle_ex(Class, Error, STrace, Req, State) -> Res when
  Class  :: error | exit | throw,
  Error  :: any(),
  STrace :: list(),
  Req    :: cowboy_req:req(),
  State  :: any(),
  Res    :: {stop, cowboy_req:req(), any()}.
handle_ex(throw, {conflict, _, _}, _STrace, Req, State) ->
  Req1 = fiar_error_view:render(conflict, Req),
  Req2 = cowboy_req:reply(409, Req1),
  {stop, Req2, State};

handle_ex(throw, {validation_errors, Errors}, _STrace, Req, State) ->
  Req1 = fiar_error_view:render({validation_errors, Errors}, Req),
  Req2 = cowboy_req:reply(422, Req1),
  {stop, Req2, State};

handle_ex(Class, Error, STrace, Req, State) ->
  _ = lager:error("Unhandled ~p:~p~nStacktrace: ~p~n", [Class, Error, STrace]),
  Req1 = fiar_error_view:render(service_unavailable, Req),
  Req2 = cowboy_req:reply(503, Req1),
  {stop, Req2, State}.
