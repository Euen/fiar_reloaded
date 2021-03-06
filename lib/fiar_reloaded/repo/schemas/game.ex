defmodule FiarReloaded.Repo.Schemas.Game do
  alias FiarReloaded.Repo.Schemas.User
  alias FiarReloaded.Repo.Boards

  @type result() :: :won | :drawn | :next
  @type t() :: %__MODULE__{
          :board => Board.t(),
          :player1 => User.t(),
          :player2 => User.t(),
          :next_chip => Board.chip(),
          :last_row_played => integer(),
          :last_col_played => integer()
        }

  @enforce_keys [:player1, :player2]
  defstruct [
    :player1,
    :player2,
    :last_row_played,
    :last_col_played,
    next_chip: 1,
    board: Boards.empty_board()
  ]
end
