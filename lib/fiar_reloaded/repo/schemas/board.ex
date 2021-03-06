defmodule FiarReloaded.Repo.Schemas.Board do

  @type chip() :: 1 | 2
  @type column() :: [chip()]
  @type row() :: [chip() | nil]
  @type diagonal() :: [chip() | nil]
  @type board() :: {column(), column(), column(), column(), column(), column(), column()}

  @type col_number() :: 1..7
  @type row_number() :: 1..6

  @type t :: %__MODULE__{
          :state => board(),
          :number_of_columns => 7,
          :number_of_rows => 6
        }

  @empty_board {[], [], [], [], [], [], []}

  defstruct state: @empty_board, number_of_columns: 7, number_of_rows: 6
end
