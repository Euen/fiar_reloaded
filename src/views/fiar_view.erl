-module(fiar_view).

%% API
-export([
  render/4
]).

%%%===================================================================
%%% Callbacks
%%%===================================================================

-callback render(Template :: atom(), Resource :: any()) -> any().

%%%===================================================================
%%% API
%%%===================================================================

-spec render(module(), atom(), any(), cowboy_req:req()) -> cowboy_req:req().
render(View, Template, Resource, Req) ->
  Body = fiar_utils:json_encode(View:render(Template, Resource)),
  cowboy_req:set_resp_body(Body, Req).