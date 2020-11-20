defmodule FiarReloaded.Repo.Schemas.PlayerTest do
  use ExUnit.Case, async: true
  alias FiarReloaded.Repo.Schemas.Player

  test "new/1 generates a Player struct" do
    assert %Player{name: "Player Name"} = %Player{name: "Player Name"}
  end
end
