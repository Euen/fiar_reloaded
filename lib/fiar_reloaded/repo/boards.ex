defmodule FiarReloaded.Repo.Boards do
  alias FiarReloaded.Repo.Schemas.Board

  @spec empty_board() :: Board.t()
  def empty_board(), do: %Board{}

  @spec get_column(Board.col_number(), Board.t()) :: Board.column()
  def get_column(col_number, board), do: elem(board.state, col_number - 1)

  @spec update_column(Board.column(), Board.col_number(), Board.t()) :: Board.t()
  def update_column(column, col_number, board) do
    updated_state = put_elem(board.state, col_number - 1, column)
    %{board | state: updated_state}
  end

  @spec get_row(Board.row_number(), Board.t()) :: Board.row()
  def get_row(row_number, board) do
    columns = Tuple.to_list(board.state)
    for c <- columns, do: Enum.reverse(c) |> Enum.at(row_number - 1)
  end

  @spec get_left_diag(Board.col_number(), Board.row_number(), Board.t()) :: Board.diagonal()
  def get_left_diag(col_number, row_number, board) do
    up_left_diag = get_up_left_diag(col_number, row_number, board, [])
    down_right_diag = get_down_right_diag(col_number, row_number, board, [])
    up_left_diag ++ tl(Enum.reverse(down_right_diag))
  end

  @spec get_right_diag(Board.col_number(), Board.row_number(), Board.t()) :: Board.diagonal()
  def get_right_diag(col_number, row_number, board) do
    up_right_diag = get_up_right_diag(col_number, row_number, board, [])
    down_left_diag = get_down_left_diag(col_number, row_number, board, [])
    up_right_diag ++ tl(Enum.reverse(down_left_diag))
  end

  @spec is_full?(Board.t()) :: boolean()
  def is_full?(board) do
    Tuple.to_list(board.state)
    |> Enum.all?(&(length(&1) == 6))
  end

  defp get_up_left_diag(cn, rn, board, acc) when cn == 1 or rn == 6 do
    chip = get_chip(cn, rn, board)
    [chip | acc]
  end

  defp get_up_left_diag(cn, rn, board, acc) do
    chip = get_chip(cn, rn, board)
    get_up_left_diag(cn - 1, rn + 1, board, [chip | acc])
  end

  defp get_down_right_diag(cn, rn, board, acc) when cn == 7 or rn == 1 do
    chip = get_chip(cn, rn, board)
    [chip | acc]
  end

  defp get_down_right_diag(cn, rn, board, acc) do
    chip = get_chip(cn, rn, board)
    get_down_right_diag(cn + 1, rn - 1, board, [chip | acc])
  end

  defp get_up_right_diag(cn, rn, board, acc) when cn == 7 or rn == 6 do
    chip = get_chip(cn, rn, board)
    [chip | acc]
  end

  defp get_up_right_diag(cn, rn, board, acc) do
    chip = get_chip(cn, rn, board)
    get_up_right_diag(cn + 1, rn + 1, board, [chip | acc])
  end

  defp get_down_left_diag(cn, rn, board, acc) when cn == 1 or rn == 1 do
    chip = get_chip(cn, rn, board)
    [chip | acc]
  end

  defp get_down_left_diag(cn, rn, board, acc) do
    chip = get_chip(cn, rn, board)
    get_down_left_diag(cn - 1, rn - 1, board, [chip | acc])
  end

  defp get_chip(col_number, row_number, board) do
    elem(board.state, col_number - 1)
    |> Enum.reverse()
    |> Enum.at(row_number - 1)
  end
end
