defmodule FiarReloaded.Test.BoardHelper do
  alias FiarReloaded.Core
  alias FiarReloaded.Repo.{Users, Games}
  alias FiarReloaded.Repo.Schemas.{Game, Board}

  @spec almost_filled_board() :: Game.t()
  def almost_filled_board() do
    # fill the board exept for the column 6
    Enum.reduce([1, 4, 2, 5], new_game(), &fill_column/2)
    |> drop_chips(5, 6)
    |> drop_chips(6, 7)
    |> drop_chips(6, 3)
  end

  @spec fill_column(Board.col_number(), Game.t()) :: Game.t()
  def fill_column(col_number, game), do: drop_chips(game, 6, col_number)

  @spec drop_chips(Game.t(), pos_integer(), Board.col_number()) :: Game.t()
  def drop_chips(game, 0, _col_number), do: game

  def drop_chips(game, n, col_number) do
    {:next, game} = Core.play(game, col_number)
    drop_chips(game, n - 1, col_number)
  end

  @spec drop_chips(Game.t(), [Board.col_number()]) :: Game.t()
  def drop_chips(game, []), do: game

  def drop_chips(game, [col_number | tail]) do
    {:next, game} = Core.play(game, col_number)
    drop_chips(game, tail)
  end

  def new_game() do
    p1 = Users.create_user(%{username: "p1", password: "password"})
    p2 = Users.create_user(%{username: "p2", password: "password"})
    Games.new(p1.username, p2.username)
  end
end
