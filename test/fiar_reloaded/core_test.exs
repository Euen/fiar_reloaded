defmodule FiarReloaded.CoreTest do
  use ExUnit.Case, async: true
  alias FiarReloaded.Core
  alias FiarReloaded.Repo.Schemas.{Game, Board, Player}
  alias FiarReloaded.Test.BoardHelper

  test "start_game/2 starts a new game with the given player names" do
    assert %Game{
             board: %Board{state: {[], [], [], [], [], [], []}},
             next_chip: 1,
             player1: %Player{name: "Player1"},
             player2: %Player{name: "Player2"}
           } == Core.start_game("Player1", "Player2")
  end

  describe "play/2" do
    setup :create_game

    test "play in an invalid column", %{game: game} do
      assert {:error, :invalid_column} = Core.play(game, 8)
    end

    test "play in an full column", %{game: game} do
      game = BoardHelper.fill_column(2, game)
      assert {:error, :full_column} = Core.play(game, 2)
    end

    test "play and wait for the next turn", %{game: game} do
      assert {:next, %Game{board: board, next_chip: 2}} = Core.play(game, 2)
      assert %Board{state: {[], [1], [], [], [], [], []}} == board
    end

    test "play and win vertically", %{game: game} do
      game = BoardHelper.drop_chips(game, [1, 2, 1, 2, 1, 2])
      assert {:won, %Game{board: board, next_chip: 2}} = Core.play(game, 1)
      assert %Board{state: {[1, 1, 1, 1], [2, 2, 2], [], [], [], [], []}} == board
    end

    test "play and win horizontally", %{game: game} do
      game = BoardHelper.drop_chips(game, [1, 1, 2, 2, 3, 3])
      assert {:won, %Game{board: board, next_chip: 2}} = Core.play(game, 4)
      assert %Board{state: {[2, 1], [2, 1], [2, 1], [1], [], [], []}} == board
    end

    test "play and win with a right diagonal", %{game: game} do
      game = BoardHelper.drop_chips(game, [4, 4, 4, 4, 6, 3, 3, 3, 2, 2, 7])
      assert {:won, %Game{board: board, next_chip: 1}} = Core.play(game, 1)
      assert %Board{state: {[2], [2, 1], [2, 1, 2], [2, 1, 2, 1], [], [1], [1]}} == board
    end

    test "play and win with a left diagonal", %{game: game} do
      game = BoardHelper.drop_chips(game, [4, 4, 4, 4, 2, 5, 5, 5, 6, 6, 1])
      assert {:won, %Game{board: board, next_chip: 1}} = Core.play(game, 7)
      assert %Board{state: {[1], [1], [], [2, 1, 2, 1], [2, 1, 2], [2, 1], [2]}} == board
    end

    test "play and end in a drawn match" do
      game = BoardHelper.almost_filled_board()
      assert {:drawn, %Game{next_chip: 1}} = Core.play(game, 6)
    end
  end

  def create_game(cx), do: Map.put(cx, :game, Core.start_game("p1", "p2"))
end
