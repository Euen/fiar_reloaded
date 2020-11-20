defmodule FiarReloaded.Repo.Schemas.Game do
  alias FiarReloaded.Repo.Schemas.{Player, Board}

  @type result() :: :won | :drawn | :next
  @type t() :: %__MODULE__{
          :board => Board.t(),
          :player1 => Player.t(),
          :player2 => Player.t(),
          :next_chip => Board.chip()
        }

  @enforce_keys [:player1, :player2, :next_chip, :board]
  defstruct [:player1, :player2, next_chip: 1, board: %Board{}]

end