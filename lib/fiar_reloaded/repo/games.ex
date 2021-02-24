defmodule FiarReloaded.Repo.Games do
  alias FiarReloaded.Repo.{Users, Boards}
  alias FiarReloaded.Repo.Schemas.{Board, Game}

  @spec new(String.t(), String.t()) :: Game.t()
  def new(player1_name, player2_name) do
    %Game{
      :player1 => Users.new(player1_name),
      :player2 => Users.new(player2_name),
      :board => Boards.empty_board(),
      :next_chip => 1
    }
  end

  @spec update_board(Board.t(), integer(), integer(), Game.t()) :: Game.t()
  def update_board(board, last_row_played, last_col_played, game) do
    %{game | board: board, last_row_played: last_row_played, last_col_played: last_col_played}
    |> update_next_chip()
  end

  @spec diff_chip(Board.chip()) :: Board.chip()
  def diff_chip(1), do: 2
  def diff_chip(2), do: 1

  defp update_next_chip(game) do
    %{game | next_chip: diff_chip(game.next_chip)}
  end
end
