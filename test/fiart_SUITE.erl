-module(fiart_SUITE).
-author('euenlopez@gmail.com').

-include_lib("common_test/include/ct.hrl").
-include_lib("mixer/include/mixer.hrl").
-mixin([{fiart_ct, [init_per_suite/1, end_per_suite/1]}]).

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([
  create_user/1,
  create_duplicate_user/1,
  start_match/1,
  play_next/1,
  play_win/1,
  play_drawn/1,
  play_wrong_player/1,
  play_invalid/1,
  current_matches/1
]).

%%====================================================================
%% CT
%%====================================================================

all() ->
  [
    create_user,
    create_duplicate_user,
    start_match,
    play_next,
    play_win,
    play_drawn,
    play_wrong_player,
    play_invalid,
    current_matches
  ].

init_per_testcase(Case, Config) when Case == play_next;
                                     Case == play_win;
                                     Case == play_drawn;
                                     Case == play_wrong_player;
                                     Case == play_invalid ->
  [UId1, UId2] = fiart_db_utils:create_users(2, id),
  MatchId = fiar:start_match(UId1, UId2),
  [{user_id1, UId1}, {user_id2, UId2}, {match_id, MatchId} | Config];
init_per_testcase(_, Config) ->
  Config.

end_per_testcase(_, Config) ->
  Config.

%%====================================================================
%% Test Cases
%%====================================================================

%% @doc test the creation of the user
create_user(_Config) ->
  User1 = fiar:create_user(<<"marcos">>, <<"pass1">>),
  UserId = fiar_user:get(id, User1),
  <<"marcos">> = fiar_user:get(username, User1),
  <<"pass1">> = fiar_user:get(password, User1),

  User1 = fiar_user_repo:find_by_id(UserId).

%% @doc test the creation of a duplicated user
create_duplicate_user(_Config) ->
  User = fiar:create_user(<<"Juan">>, <<"pass1">>),
  User = fiar_user_repo:find_by_id(fiar_user:get(id, User)),
  UserId = fiar_user:get(id, User),

  {conflict, fiar_user, UserId} = (catch fiar:create_user(<<"Juan">>, <<"pass1">>)).

%% @doc test the creation of the game
start_match(_Config) ->
  [UId1, UId2] = fiart_db_utils:create_users(2, id),
  MId = fiar:start_match(UId1, UId2),
  true = is_pid(whereis(MId)),

  %% Starts a match that is already started
  MId = fiar:start_match(UId1, UId2).

%% @doc test the functionality to play and the first state of the board
play_next(Config) ->
  MatchId = ?config(match_id, Config),
  UserId = ?config(user_id1, Config),
  {next, #{board := {
      [1], [], [], [], [], [], []
  }}} = fiar:play(MatchId, 1, UserId).

%% @doc test the functionality to play and win
play_win(Config) ->
  MatchId = ?config(match_id, Config),
  UserId1 = ?config(user_id1, Config),
  UserId2 = ?config(user_id2, Config),
  ok = drop_chips([1, 2, 1, 2, 1, 2], MatchId, UserId1, UserId2),
  {won, _} = fiar:play(MatchId, 1, UserId1).

%% @doc test the functionality to play and end in a draw
play_drawn(Config) ->
  MatchId = ?config(match_id, Config),
  UserId1 = ?config(user_id1, Config),
  UserId2 = ?config(user_id2, Config),
  ok = almost_fill_board(MatchId, UserId1, UserId2),
  {drawn, _} = fiar:play(MatchId, 4, UserId2).

%% @doc test what happends if a play is made by a wrong player
play_wrong_player(Config) ->
  MatchId = ?config(match_id, Config),
  UserId = ?config(user_id1, Config),
  {next, _} = fiar:play(MatchId, 1, UserId),
  {error, wrong_player} = fiar:play(MatchId, 1, UserId).

%% @doc test the error handling of the fiar_match gen_server
play_invalid(Config) ->
  MatchId = ?config(match_id, Config),
  UserId = ?config(user_id1, Config),
  {error, invalid_column} = fiar:play(MatchId, 8, UserId).

current_matches(_Config) ->
  User1 = fiar_user:get(id, fiar:create_user(<<"Iñaki">>, <<"pass1">>)),
  User2 = fiar_user:get(id, fiar:create_user(<<"Federico">>, <<"pass1">>)),
  User3 = fiar_user:get(id, fiar:create_user(<<"Hernan">>, <<"pass1">>)),
  User4 = fiar_user:get(id, fiar:create_user(<<"Euen">>, <<"pass1">>)),

  Mid1 = fiar:start_match(User1, User2),
  Mid2 = fiar:start_match(User1, User3),

  Match1 = fiar_match:fetch(Mid1),
  Match2 = fiar_match:fetch(Mid2),

  true = lists:member(Match1, fiar:current_matches(User1)),
  true = lists:member(Match2, fiar:current_matches(User1)),
  [Match1] = fiar:current_matches(User2),
  [Match2] = fiar:current_matches(User3),
  [] = fiar:current_matches(User4).

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
%% @doc fills all columns in the board except #4
% 6 |o x o _ x o x
% 5 |o x o x x o x
% 4 |o x o o x o x
% 3 |x o x x o x o
% 2 |x o x o o x o
% 1 |x o x x o x o
%   --------------
%    1 2 3 4 5 6 7
almost_fill_board(MatchId, UserId1, UserId2) ->
  AlmostFullBoard =
    [
      1, 2, 1, 2, 1, 2, 2, 1, 2, 1, 2, 1,
      3, 5, 3, 5, 3, 5, 5, 3, 5, 3, 5, 3,
      6, 7, 6, 7, 6, 7, 7, 6, 7, 6, 7, 6,
      4, 4, 4, 4, 4
    ],
  drop_chips(AlmostFullBoard, MatchId, UserId1, UserId2).

%% @private
drop_chips([], _MatchId, _CurrentPlayer, _NextPlayer) -> ok;
drop_chips([Col | Rest], MatchId, CurrentPlayer, NextPlayer) ->
  {next, _} = fiar:play(MatchId, Col, CurrentPlayer),
  drop_chips(Rest, MatchId, NextPlayer, CurrentPlayer).