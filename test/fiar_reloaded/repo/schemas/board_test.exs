defmodule FiarReloaded.BoardTest do
  use ExUnit.Case, async: true
  alias FiarReloaded.Repo.Schemas.Board

  test "empty Board struct" do
    assert %Board{state: {[], [], [], [], [], [], []}} == %Board{}
  end
end
