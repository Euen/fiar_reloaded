-module(fiar_error_view).

-behaviour(fiar_view).

%% View callbacks
-export([
  render/2
]).

%%%===================================================================
%%% validation errors
%%%===================================================================

render(error, {validation_errors, Errors}) ->
  error_response(Errors);

%%%===================================================================
%%% Http errors
%%%===================================================================

render(error, unauthorized) ->
  error_response(<<"user does not have the necessary credentials">>);

render(error, conflict) ->
  error_response(<<"the request couldn't be processed due to a resource conflict">>);

render(error, service_unavailable) ->
  error_response(<<"the server is temporarily unavailable">>);

%%%===================================================================
%%% API
%%%===================================================================

render(Error, Req) ->
  fiar_view:render(?MODULE, error, Error, Req).

%%%===================================================================
%%% Helpers
%%%===================================================================

error_response(Errors) when is_list(Errors) ->
  #{errors => Errors};

error_response(Msg) ->
  #{errors => [#{message => Msg}]}.