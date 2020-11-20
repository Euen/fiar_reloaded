defmodule FiarReloaded.GameTest do
  use ExUnit.Case, async: true
  alias FiarReloaded.Repo.Games
  alias FiarReloaded.Repo.Schemas.{Game, Board, Player}

  test "generates a Game struct" do
    assert %Game{
             board: %Board{state: {[], [], [], [], [], [], []}},
             next_chip: 1,
             player1: %Player{name: "Player1"},
             player2: %Player{name: "Player2"}
           } == Games.new("Player1", "Player2")
  end

  test "update_board/2 update the Game with the given Board" do
    game = Games.new("Player1", "Player2")
    board = %Board{state: {[], [1], [], [], [], [], []}}

    assert %Game{
             board: %Board{state: {[], [1], [], [], [], [], []}},
             next_chip: 2,
             player1: %Player{name: "Player1"},
             player2: %Player{name: "Player2"}
           } == Games.update_board(board, game)
  end

  test "diff_chip/1 toggle between chips" do
    assert 1 == Games.diff_chip(2)
    assert 2 == Games.diff_chip(1)
  end
end
