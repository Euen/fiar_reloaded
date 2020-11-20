defmodule FiarReloaded.Repo.PlayerTest do
  use ExUnit.Case, async: true
  alias FiarReloaded.Repo.{Players, Schemas.Player}

  test "new/1 generates a Player struct" do
    assert %Player{name: "Player Name"} = Players.new("Player Name")
  end
end
