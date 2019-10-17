-module(fiar_core).

-export([new_match/3, play/2]).
-export([match_to_map/1, match_id/2]).

-define(LAST_COLUMN_NUMBER, 7).
-define(LAST_ROW_NUMBER, 6).

-type user_id()  :: fiar_user:id().
-type chip()     :: 1 | 2.
-type column()   :: [chip()].
-type col_num()  :: 1..7.
-type row_num()  :: 1..6.

-opaque board()    :: {column(), column(), column(), column(), column(), column(), column()}.
-opaque result()   :: won | drawn | next.
-opaque match_id() :: atom().
-opaque match()    ::
  #{
    id => match_id(),
    player_id1 => user_id(),
    player_id2 => user_id(),
    board => board(),
    next_chip => chip()
  }.

-export_type([match/0, match_id/0, col_num/0, board/0]).
-export_type([result/0]).

%%====================================================================
%% API - Play
%%====================================================================

-spec new_match(match_id(), user_id(), user_id()) -> match().
new_match(MatchId, UserId1, UserId2) ->
  #{
    id => MatchId,
    player_id1 => UserId1,
    player_id2 => UserId2,
    board => {[], [], [], [], [], [], []},
    next_chip => 1
  }.

-spec play(col_num(), match()) -> {result(), match()} | no_return().
play(Col, _) when Col < 1 orelse Col > ?LAST_COLUMN_NUMBER ->
  throw(invalid_column);
play(Col, Match) ->
  Board    = fiar_match:get(board, Match),
  NextChip = fiar_match:get(next_chip, Match),
  Column   = element(Col, Board),

  NewColumn = [NextChip|Column],
  NewBoard  = setelement(Col, Board, NewColumn),
  NewMatch0 = fiar_match:set(board, NewBoard, Match),
  NewMatch  = fiar_match:set(next_chip, diff_chip(NextChip), NewMatch0),
  %% change the [_, _, ..] allow to change the board size in just one place
  case Column of
    [_, _, _, _, _, _] ->
      throw(invalid_row);
    [NextChip, NextChip, NextChip | _] ->
      {won, NewMatch};
    _ ->
      Status = analyze(Col, NewColumn, NextChip, NewBoard),
      {Status, NewMatch}
  end.

%%====================================================================
%% API - Data manipulation
%%====================================================================

-spec match_to_map(match()) -> map().
match_to_map(#{board := Board} = Match) ->
  ListBoard = board_to_list(Board),
  maps:fold(fun
    (board, _V, ViewMatch) ->
      ViewMatch#{<<"board">> => ListBoard};
    (id, V, ViewMatch) ->
      ViewMatch#{<<"id">> => atom_to_binary(V, utf8)};
    (K, V, ViewMatch) ->
      ViewMatch#{atom_to_binary(K, utf8) => V}
  end, #{}, Match).

-spec match_id(user_id(), user_id()) -> match_id().
match_id(UserId1, UserId2) ->
  BMI = case UserId1 < UserId2 of
    true ->
      <<"m_", (fiar_user:id_to_bin(UserId1))/binary, "_", (fiar_user:id_to_bin(UserId2))/binary>>;
    false ->
      <<"m_", (fiar_user:id_to_bin(UserId2))/binary, "_", (fiar_user:id_to_bin(UserId1))/binary>>
  end,
  binary_to_atom(BMI, utf8).

%%====================================================================
%% Internal Functions
%%====================================================================

-spec board_to_list(board()) -> list().
board_to_list({C1, C2, C3, C4, C5, C6, C7}) ->
  [C1, C2, C3, C4, C5, C6, C7].

-spec diff_chip(chip()) -> chip().
diff_chip(1) -> 2;
diff_chip(2) -> 1.

-spec analyze(col_num(), column(), chip(), board()) -> result().
analyze(Col, Column, Chip, Board) ->
  RowNum = length(Column),
  case wins_row(RowNum, Chip, Board) orelse
       wins_left_diag(Col, RowNum, Chip, Board) orelse
       wins_right_diag(Col, RowNum, Chip, Board) of
    true ->
      won;
    false ->
      case is_full(Board) of
          true -> drawn;
          false -> next
      end
  end.

-spec wins_row(row_num(), chip(), board()) -> boolean().
wins_row(RowNum, Chip, Board) ->
  Row = get_row(RowNum, Board),
  contains_four(Chip, Row).

-spec contains_four(chip(), column()) -> boolean().
contains_four(_Chip, List) when length(List) < 4 -> false;
contains_four(Chip, [Chip, Chip, Chip, Chip | _ ])  -> true;
contains_four(Chip, [ _ | Rest ]) -> contains_four(Chip, Rest).

-spec get_row(row_num(), board()) -> [chip()].
get_row(RowNum, Board) ->
  Columns = tuple_to_list(Board),
  lists:map(fun(Column) -> get_chip(RowNum, Column) end, Columns).

-spec get_left_diag(col_num(), row_num(), board()) -> [chip()].
get_left_diag(Col, RowNum, Board) ->
  UpLeftDiag = get_up_left_diag(Col, RowNum, Board, []),
  DownLeftDiag = get_down_left_diag(Col, RowNum, Board, []),
  UpLeftDiag ++ tl(lists:reverse(DownLeftDiag)).

-spec get_up_left_diag(col_num(), row_num(), board(), list()) -> [chip()].
get_up_left_diag(Col, RowNum, Board, Acc) when Col =:= 1; RowNum =:= ?LAST_ROW_NUMBER ->
  Chip = get_chip(Col, RowNum, Board),
  [Chip | Acc];
get_up_left_diag(Col, RowNum, Board, Acc) ->
  Chip = get_chip(Col, RowNum, Board),
  Next = [Chip | Acc],
  get_up_left_diag(Col-1, RowNum+1, Board, Next).

-spec get_down_left_diag(col_num(), row_num(), board(), list()) -> [chip()].
get_down_left_diag(?LAST_COLUMN_NUMBER, RowNum, Board, Acc) ->
  Chip = get_chip(?LAST_COLUMN_NUMBER, RowNum, Board),
  [Chip | Acc];
get_down_left_diag(Col, 1, Board, Acc) ->
  Chip = get_chip(Col, 1, Board),
  [Chip | Acc];
get_down_left_diag(Col, RowNum, Board, Acc) ->
  Chip = get_chip(Col, RowNum, Board),
  Next = [Chip | Acc],
  get_down_left_diag(Col + 1, RowNum - 1, Board, Next).

-spec get_right_diag(col_num(), row_num(), board()) -> [chip()].
get_right_diag(Col, RowNum, Board) ->
  DownRightDiag = get_down_right_diag(Col, RowNum, Board, []),
  UpRightDiag = get_up_right_diag(Col, RowNum, Board, []),
  DownRightDiag ++ tl(lists:reverse(UpRightDiag)).

-spec get_down_right_diag(col_num(), row_num(), board(), list()) -> [chip()].
get_down_right_diag(Col, RowNum, Board, Acc) when Col =:= 1; RowNum =:= 1 ->
  Chip = get_chip(Col, RowNum, Board),
  [Chip | Acc];
get_down_right_diag(Col, RowNum, Board, Acc) ->
  Chip = get_chip(Col, RowNum, Board),
  Next = [Chip | Acc],
  get_down_right_diag(Col - 1, RowNum - 1, Board, Next).

-spec get_up_right_diag(col_num(), row_num(), board(), list()) -> [chip()].
get_up_right_diag(Col, RowNum, Board, Acc) when Col =:= ?LAST_COLUMN_NUMBER;
                                                RowNum =:= ?LAST_ROW_NUMBER ->
  Chip = get_chip(Col, RowNum, Board),
  [Chip | Acc];
get_up_right_diag(Col, RowNum, Board, Acc) ->
  Chip = get_chip(Col, RowNum, Board),
  Next = [Chip | Acc],
  get_up_right_diag(Col + 1, RowNum + 1, Board, Next).

-spec get_chip(row_num(), column()) -> chip() | 0.
get_chip(RowNum, Column) when length(Column) >= RowNum ->
  lists:nth(RowNum, lists:reverse(Column));
get_chip(_RowNum, _Column) -> 0.

-spec get_chip(col_num(), row_num(), board()) -> chip() | 0.
get_chip(Col, RowNum, Board) ->
  Columns = tuple_to_list(Board),
  Column = lists:nth(Col, Columns),
  get_chip(RowNum, Column).

-spec wins_left_diag(col_num(), row_num(), chip(), board()) -> boolean().
wins_left_diag(Col, RowNum, Chip, Board) ->
  Diag = get_left_diag(Col, RowNum, Board),
  contains_four(Chip, Diag).

-spec wins_right_diag(col_num(), row_num(), chip(), board()) -> boolean().
wins_right_diag(Col, RowNum, Chip, Board) ->
  Diag = get_right_diag(Col, RowNum, Board),
  contains_four(Chip, Diag).

-spec is_full(board()) -> boolean().
is_full(Board) ->
  Columns = tuple_to_list(Board),
  Fun = fun(Col) ->
         case length(Col) of
               6 -> true;
               _ -> false
          end
  end,
  lists:all(Fun, Columns).