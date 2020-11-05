defmodule FiarReloded.Board do
  @type chip() :: 1 | 2
  @type col() :: chip() | :empty
  @type row() :: {col(), col(), col(), col(), col(), col(), col()}
  @type board() :: {row(), row(), row(), row(), row(), row()}

  @type t :: %__MODULE__{
          :board => board()
        }

  @empty_board {
    {:empty, :empty, :empty, :empty, :empty, :empty, :empty},
    {:empty, :empty, :empty, :empty, :empty, :empty, :empty},
    {:empty, :empty, :empty, :empty, :empty, :empty, :empty},
    {:empty, :empty, :empty, :empty, :empty, :empty, :empty},
    {:empty, :empty, :empty, :empty, :empty, :empty, :empty},
    {:empty, :empty, :empty, :empty, :empty, :empty, :empty}
  }

  defstruct board: @empty_board

  @spec empty_board() :: Board.t()
  def empty_board(), do: @empty_board
end

# defmodule FiarReloded.Games do
#   alias FiarReloded.{Player, Board}

#   @type t :: %__MODULE__{
#           :board => Board.t(),
#           :player1 => Player.t(),
#           :player2 => Player.t()
#         }

#   defstruct [:board, :player1, :player2]
# end
