-module(fiar_validator).

-export([
  run/2,
  raise/3,
  raise/1,
  regex/1
]).

-type field_name() :: binary() | atom().
-type options() :: #{error_opts := #{code := integer(), template_params => map()}}.
-type operator() :: less_than |
                    greater_than |
                    less_than_or_equal_to |
                    greater_than_or_equal_to |
                    equal_to.

-type validation() :: {required,    [field_name()]} |
                      {required,    [field_name()], options()} |
                      {integer,     field_name()} |
                      {integer,     field_name(), options()} |
                      {number,      field_name(), [{operator(), number()}]} |
                      {number,      field_name(), [{operator(), number()}], options()} |
                      {string,      field_name()} |
                      {string,      field_name(), options()} |
                      {format,      field_name(), binary()} |
                      {format,      field_name(), binary(), options()} |
                      {inclusion,   field_name(), [any()]} |
                      {inclusion,   field_name(), [any()], options()} |
                      {format_list, [#{name => any(), validation => binary()}]} |
                      {format_list, [#{name => any(), validation => binary()}], options()}.

-type validations() :: [validation()].

%%%===================================================================
%%% API
%%%===================================================================

-spec run(map(), validations()) -> ok | no_return().
run(Params, Validations) ->
  Result = lists:foldl(fun(Validation, Acc) ->
    case do_validate(Params, Validation) of
      true  -> Acc;
      []    -> Acc;
      Error -> [Error | Acc]
    end
  end, [], Validations),
  raise_if_error(Result).

-spec raise(atom() | binary(), atom(), binary()) -> no_return().
raise(Field, Type, Msg) ->
  % ErrOpts = maybe_add_error_options(#{}, validator_raise_error, #{field => Field}),
  raise(error_response(Field, Type, {final_msg, Msg})).

-spec raise(map() | [map()]) -> no_return().
raise(Errors) when is_list(Errors) ->
  throw({validation_errors, Errors});
raise(Error) ->
  raise([Error]).

%%%===================================================================
%%% Regular Expresions
%%%===================================================================

regex(phrase) -> <<"^([A-Za-z\s]{0,50})$">>;
regex(word) -> <<"^([A-Za-z0-9ñÑ]{0,50})$">>;
regex(pass) -> <<"^([A-Za-z0-9]{0,50})$">>;
regex(email) -> <<"\\b[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9-]+\\.[a-z]{2,4}\\b">>.

%%%===================================================================
%%% Validations
%%%===================================================================

do_validate(Params, {required, Fields}) ->
  do_validate(Params, {required, Fields, #{}});

do_validate(Params, {required, Fields, _Options}) ->
  lists:foldl(fun(RequiredField, Acc) ->
    case maps:get(RequiredField, Params, undefined) of
      undefined ->
        [error_response(RequiredField, required, <<"can't be blank">>) | Acc];
      _ ->
        Acc
    end
  end, [], Fields);

do_validate(Params, {integer, Field}) ->
  do_validate(Params, {integer, Field, #{}});

do_validate(Params, {integer, Field, _Options}) ->
  case maps:get(Field, Params, undefined) of
    undefined -> true;
    Value when is_integer(Value) -> true;
    _ ->
      error_response(Field, integer, <<"must be integer">>)
  end;

do_validate(Params, {string, Field}) ->
  do_validate(Params, {string, Field, #{}});

do_validate(Params, {string, Field, _Options}) ->
  case maps:get(Field, Params, undefined) of
    undefined -> true;
    Value when is_binary(Value) -> true;
    _ ->
      error_response(Field, string, <<"must be string">>)
  end;

%% Keep in mind that the NumberValidators are going to be evaluated with a boolean AND formula between
%% the NumberValidators
do_validate(Params, {number, Field, NumberValidators}) ->
  do_validate(Params, {number, Field, NumberValidators, #{}});

do_validate(Params, {number, Field, NumberValidators, _Options}) ->
  case maps:get(Field, Params, undefined) of
    undefined ->
      true;
    Value when is_number(Value) ->
      lists:foldl(fun({ValidationKey, TargetValue}, Acc) ->
        {ok, {ValidationFun, Msg}} = maps:find(ValidationKey, number_validators(TargetValue)),
        ErrorRes = error_response(Field, number, Msg),
        maybe_add_error(ValidationFun(Value, TargetValue), ErrorRes, Acc)
      end, [], NumberValidators);
    _ ->
      error_response(Field, number, <<"must be number">>)
  end;

do_validate(Params, {format, Field, Format}) ->
  do_validate(Params, {format, Field, Format, #{}});

do_validate(Params, {format, Field, Format, _Options}) ->
  case maps:get(Field, Params, undefined) of
    undefined ->
      true;
    Value ->
      case re:run(fiar_utils:to_bin(Value), Format) of
        nomatch ->
          error_response(Field, format, <<"has invalid format">>);
        _ -> true
      end
  end;

do_validate(Params, {format_list, Fields}) ->
  do_validate(Params, {format_list, Fields, #{}});

do_validate(Params, {format_list, Fields, Options}) ->
  lists:foldl(fun(#{name := Name, validation := Format}, Acc) ->
    Field = fiar_utils:to_bin(Name),
    case maps:get(Field, Params, undefined) of
      undefined ->
        [error_response(Field, required, <<"can't be blank">>) | Acc];
      _ ->
        Result = do_validate(Params, {format, Field, Format, Options}),
        maybe_add_error(Result, Result, Acc)
    end
  end, [], Fields);

do_validate(Params, {inclusion, Field, TargetFields}) ->
  do_validate(Params, {inclusion, Field, TargetFields, #{}});

do_validate(Params, {inclusion, Field, TargetFields, _Options}) ->
  case maps:get(Field, Params, undefined) of
    undefined ->
      error_response(Field, inclusion, <<"can't be blank">>);
    Value ->
      case lists:member(Value, TargetFields) of
        true -> true;
        _ ->
          BinOpts = fiar_utils:binary_join([fiar_utils:to_bin(O) ||  O <- TargetFields], <<", ">>),
          error_response(Field, inclusion, <<"must be one of ", BinOpts/binary, "">>)
      end
  end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

maybe_add_error(true, _, Acc) ->
  Acc;
maybe_add_error(_, Error, Acc) ->
  [Error | Acc].

number_validators(N) ->
  Text = fun(T) -> text("must be ~s ~p", [T, N]) end,
  #{
    less_than                => {fun(X, Y) -> X < Y end,  Text("less than")},
    greater_than             => {fun(X, Y) -> X > Y end,  Text("greater than")},
    less_than_or_equal_to    => {fun(X, Y) -> X =< Y end, Text("less than or equal to")},
    greater_than_or_equal_to => {fun(X, Y) -> X >= Y end, Text("greater than or equal to")},
    equal_to                 => {fun(X, Y) -> X == Y end, Text("equal to")}
  }.

text(Msg, Args) ->
  iolist_to_binary(io_lib:format(Msg, Args)).

raise_if_error([]) ->
  ok;
raise_if_error(Errors) ->
  raise(lists:flatten(Errors)).

error_response(Field, Type, {final_msg, Msg}) ->
  #{
    field => Field,
    type => Type,
    message => Msg
  };

error_response(Field, Type, Msg) ->
  error_response(Field, Type, {final_msg, <<Field/binary, ": ", Msg/binary>>}).

% maybe_add_error_options(Options, Code, DefaultTemplateParams) ->
%   ErrorOpts = maps:get(error_opts, Options, #{}),
%   ErrorCode = maps:get(code, ErrorOpts, Code),
%   TemplateParams = maps:get(template_params, ErrorOpts, DefaultTemplateParams),

%   Options#{
%     error_opts => ErrorOpts#{
%       code => ErrorCode,
%       template_params => TemplateParams
%     }
%   }.
