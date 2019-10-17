-module(fiar_utils).

-export([
  json_encode/1,
  json_decode/1,
  to_bin/1,
  binary_join/1,
  binary_join/2
]).

%% @doc this function encodes the provided data
-spec json_encode(jiffy:json_value()) -> iodata().
json_encode(Data) ->
  jiffy:encode(Data).

%% @doc this function decodes the provided data and return maps
-spec json_decode(iolist() | binary()) -> jiffy:jiffy_decode_result().
json_decode(Data) ->
  try
    jiffy:decode(Data, [return_maps])
  catch
    _:_ -> throw({bad_json, Data})
  end.

-spec to_bin(binary() | list() | atom() | integer() | float()) -> binary().
to_bin(Value) when is_binary(Value) ->
  Value;
to_bin(Value) when is_list(Value) ->
  list_to_binary(Value);
to_bin(Value) when is_atom(Value) ->
  atom_to_binary(Value, utf8);
to_bin(Value) when is_integer(Value) ->
  integer_to_binary(Value);
to_bin(Value) when is_float(Value) ->
  float_to_binary(Value, [{decimals, 2}, compact]).

%% @doc Joins and returns a list of binaries
-spec binary_join([binary()]) -> binary().
binary_join(Binaries) ->
  binary_join(Binaries, <<>>).

%% @doc Joins and returns a list of binaries
-spec binary_join([binary()], binary()) -> binary().
binary_join([], _) ->
  <<>>;
binary_join([S], _) when is_binary(S) ->
  S;
binary_join([H | T], Sep) ->
  B = << <<Sep/binary, X/binary>> || X <- T >>,
  <<H/binary, B/binary>>.