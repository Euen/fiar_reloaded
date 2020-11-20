defmodule FiarReloaded.Repo.Schemas.BoardTest do
  use ExUnit.Case, async: true
  alias FiarReloaded.Repo.Boards
  alias FiarReloaded.Repo.Schemas.Board
  alias FiarReloaded.Test.BoardHelper

  test "empty_board/0 returns an empty Board" do
    assert %Board{state: {[], [], [], [], [], [], []}} == Boards.empty_board()
  end

  test "get_column/2 returns the specified column of the Board" do
    board = %Board{state: {[], [1, 2, 1], [], [], [], [], []}}
    assert [1, 2, 1] == Boards.get_column(2, board)
  end

  test "update_column/3 update the Board with the given column" do
    assert %Board{state: {[], [1, 2, 1], [], [], [], [], []}} ==
             Boards.update_column([1, 2, 1], 2, %Board{})
  end

  test "get_row/2 returns the specified row of the Board" do
    board = %Board{state: {[2], [1, 2, 1], [1, 2, 1, 2], [1, 2, 1, 2], [1, 1], [2, 2], []}}
    assert [nil, 2, 1, 1, 1, 2, nil] == Boards.get_row(2, board)
  end

  test "get_left_diag/3 returns the left diagonal given a position" do
    # [nil, [nil, [nil, [nil, [nil, [nil, [nil,
    #  nil,  nil,  nil,  nil,  nil,  nil,  nil,
    #  nil,  nil,   1,    1,   nil,  nil,  nil,
    #  nil,   1,    2,    2,   nil,  nil,  nil,
    #  nil,   2,    1,    1,    1,    2,   nil,
    #   2]    1]    2]    2]    1]    2]   nil]
    board = %Board{state: {[2], [1, 2, 1], [1, 2, 1, 2], [1, 2, 1, 2], [1, 1], [2, 2], []}}
    assert [2] == Boards.get_left_diag(1, 1, board)
    assert [nil, 1, 1, 2] == Boards.get_left_diag(4, 1, board)
    assert [nil, 1, 1, 2] == Boards.get_left_diag(2, 3, board)
    assert [nil, nil, 1, 2, 1, 2] == Boards.get_left_diag(6, 1, board)
    assert [nil, nil] == Boards.get_left_diag(7, 5, board)
  end

  test "get_right_diag/3 returns the right diagonal given a position" do
    # [nil, [nil, [nil, [nil, [nil, [nil, [nil,
    #  nil,  nil,  nil,  nil,  nil,  nil,  nil,
    #  nil,  nil,   1,    1,   nil,  nil,  nil,
    #  nil,   1,    2,    2,   nil,  nil,  nil,
    #  nil,   2,    1,    1,    1,    2,   nil,
    #   2]    1]    2]    2]    1]    2]   nil]
    board = %Board{state: {[2], [1, 2, 1], [1, 2, 1, 2], [1, 2, 1, 2], [1, 1], [2, 2], []}}
    assert [nil, nil, 1, 2, 2, 2] == Boards.get_right_diag(1, 1, board)
    assert [nil, nil, 1, 2] == Boards.get_right_diag(4, 1, board)
    assert [nil, nil, 1, 1, nil] == Boards.get_right_diag(2, 3, board)
    assert [nil, 2] == Boards.get_right_diag(6, 1, board)
    assert [nil, nil, nil, 1, 2] == Boards.get_right_diag(7, 5, board)
  end

  test "is_full/1 returns if te Board is complete or not" do
    board = %Board{}
    refute Boards.is_full?(board)

    game = BoardHelper.almost_filled_board()
    refute Boards.is_full?(game.board)

    # in the game the board is almost full exept for the col 6 (index: 5)
    updated_state = put_elem(game.board.state, 5, [2, 1, 2, 1, 2, 1])
    full_board = %{game | board: %{game.board | state: updated_state}}
    assert Boards.is_full?(full_board.board)
  end
end
