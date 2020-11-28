defmodule FiarReloaded.Repo.Schemas.GameTest do
  use ExUnit.Case, async: true
  alias FiarReloaded.Repo.Users
  alias FiarReloaded.Repo.Schemas.{Game, Board, User}

  test "Game struct" do
    assert %Game{
             board: %Board{state: {[], [], [], [], [], [], []}},
             next_chip: 1,
             player1: Users.new("player1"),
             player2: Users.new("player2")
           } == %Game{
            board: %Board{},
            next_chip: 1,
            player1: Users.new("player1"),
            player2: Users.new("player2")
          }
  end
end
