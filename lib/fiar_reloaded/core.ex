defmodule FiarReloaded.Core do
  alias FiarReloaded.Repo.{Games, Boards}
  alias FiarReloaded.Repo.Schemas.{Game, Board}

  @spec start_game(String.t(), String.t()) :: Game.t()
  def start_game(player1_name, player2_name) do
    Games.new(player1_name, player2_name)
  end

  @spec play(Game.t(), Board.col_number()) :: {Game.result(), Game.t()} | {:error, term()}
  def play(game, col_number) do
    with :ok <- validate_col_number(col_number),
         {:ok, game} <- drop_chip(game, col_number) do
      analyze(game, col_number)
    else
      {:error, error} -> {:error, error}
    end
  end

  @spec is_user_turn(Game.t(), User.t()) :: boolean()
  def is_user_turn(game, current_user) do
    player = get_player_by_turn(game.next_chip)
    current_user.username == Map.get(game, player).username
  end

  defp get_player_by_turn(1), do: :player1
  defp get_player_by_turn(2), do: :player2

  @spec validate_col_number(integer()) :: :ok | {:error, :invalid_column}
  defp validate_col_number(col_number) when col_number in 1..7, do: :ok
  defp validate_col_number(_), do: {:error, :invalid_column}

  @spec drop_chip(Game.t(), Board.col_number()) :: {:ok, Game.t()} | {:error, term()}
  defp drop_chip(game, col_number) do
    chip = game.next_chip
    column = Boards.get_column(col_number, game.board)

    case is_column_full?(column) do
      false ->
        updated_column = [chip | column]

        updated_game =
          updated_column
          |> Boards.update_column(col_number, game.board)
          |> Games.update_board(length(updated_column), col_number, game)

        {:ok, updated_game}

      true ->
        {:error, :full_column}
    end
  end

  defp is_column_full?(column) when length(column) > 5, do: true
  defp is_column_full?(_), do: false

  defp analyze(game, col_number) do
    column = Boards.get_column(col_number, game.board)
    row_number = length(column)
    played_chip = Games.diff_chip(game.next_chip)

    with :next <- contains_four(column, played_chip),
         :next <- analyze_row(game, row_number, played_chip),
         :next <- analyze_left_diag(game, col_number, row_number, played_chip),
         :next <- analyze_right_diag(game, col_number, row_number, played_chip),
         false <- Boards.is_full?(game.board) do
      {:next, game}
    else
      true -> {:drawn, game}
      :won -> {:won, game}
    end
  end

  defp analyze_row(game, row_number, chip) do
    row_number
    |> Boards.get_row(game.board)
    |> contains_four(chip)
  end

  defp analyze_left_diag(game, col_number, row_number, chip) do
    col_number
    |> Boards.get_left_diag(row_number, game.board)
    |> contains_four(chip)
  end

  defp analyze_right_diag(game, col_number, row_number, chip) do
    col_number
    |> Boards.get_right_diag(row_number, game.board)
    |> contains_four(chip)
  end

  defp contains_four(list, _) when length(list) < 4, do: :next
  defp contains_four([ch, ch, ch, ch | _], ch), do: :won
  defp contains_four([_ | tail], ch), do: contains_four(tail, ch)
end
