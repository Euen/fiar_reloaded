defmodule FiarReloaded.Repo.Schemas.GameTest do
  use ExUnit.Case, async: true
  alias FiarReloaded.Repo.Schemas.{Game, Board, Player}

  test "Game struct" do
    assert %Game{
             board: %Board{state: {[], [], [], [], [], [], []}},
             next_chip: 1,
             player1: %Player{name: "Player1"},
             player2: %Player{name: "Player2"}
           } == %Game{
            board: %Board{},
            next_chip: 1,
            player1: %Player{name: "Player1"},
            player2: %Player{name: "Player2"}
          }
  end
end
