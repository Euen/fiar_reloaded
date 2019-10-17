-module(fiart_core_SUITE).
-author('elbrujohalcon@inaka.net').
-author('euenlopez@gmail.com').

-include_lib("common_test/include/ct.hrl").
-include_lib("mixer/include/mixer.hrl").
-mixin([{fiart_ct, [init_per_suite/1, end_per_suite/1]}]).

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([
  invalid_column/1,
  invalid_row/1,

  wins_vertically/1,
  wins_horizontally/1,
  wins_right_diagonally/1,
  wins_left_diagonally/1
]).

%%====================================================================
%% CT
%%====================================================================

all() ->
  [
    invalid_column,
    invalid_row,
    wins_vertically,
    wins_horizontally,
    wins_right_diagonally,
    wins_left_diagonally
  ].

init_per_testcase(_, Config) ->
  [{match, new_match()} | Config].

end_per_testcase(_, Config) ->
  Config.

%%====================================================================
%% Test Cases
%%====================================================================

%% @doc test play in a invalid column
invalid_column(Config) ->
  Match = ?config(match, Config),
  invalid_column = (catch fiar_core:play(8, Match)),
  invalid_column = (catch fiar_core:play(-1, Match)).

%% @doc test play in an invalid row
invalid_row(Config) ->
  Match = ?config(match, Config),
  FullColumnBoard = fill_column(1, Match),
  invalid_row = (catch fiar_core:play(1, FullColumnBoard)).

%% @doc when the player puts 4 chips in a vertical row, wins
wins_vertically(Config) ->
  Match = ?config(match, Config),
  CheckMateBoard = drop_chips([1, 2, 1, 2, 1, 2], Match),
  {won, _} = fiar_core:play(1, CheckMateBoard).

%% @doc when the player puts 4 chips in a horizontal row, wins
wins_horizontally(Config) ->
  Match = ?config(match, Config),
  CheckMateBoard = drop_chips([2, 5, 3, 6, 4, 7], Match),
  {won, _} = fiar_core:play(1, CheckMateBoard).

%% @doc when the player puts 4 chips in a diagonal row, wins
wins_right_diagonally(Config) ->
  Match = ?config(match, Config),
  CheckMateBoard = drop_chips([4, 4, 4, 4, 6, 3, 3, 3, 2, 2, 7], Match),
  {won, _} = fiar_core:play(1, CheckMateBoard).

%% @doc when the player puts 4 chips in a diagonal row, wins
wins_left_diagonally(Config) ->
  Match = ?config(match, Config),
  CheckMateBoard = drop_chips([4, 4, 4, 4, 2, 5, 5, 5, 6, 6, 1], Match),
  {won, _} = fiar_core:play(7, CheckMateBoard).

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
%% @doc fills all columns in the Match except #3
% almost_fill_board(Match) ->
  % WithColsExcept3 = lists:foldl(fun fill_column/2, Match, [1, 4, 2, 5, 6, 7]),
  % drop_chips(6, 3, WithColsExcept3).

%% @private
fill_column(Col, Match) -> drop_chips(6, Col, Match).

%% @private
drop_chips(0, _Col, Match) -> Match;
drop_chips(N, Col, Match) ->
  {Col, Match, {next, NextBoard}} = {Col, Match, fiar_core:play(Col, Match)},
  drop_chips(N-1, Col, NextBoard).

%% @private
drop_chips([], Match) -> Match;
drop_chips([Col|Rest], Match) ->
  {Col, Match, {next, NextBoard}} = {Col, Match, fiar_core:play(Col, Match)},
  drop_chips(Rest, NextBoard).

new_match() ->
  #{
    board => {[], [], [], [], [], [], []},
    next_chip => 1
  }.