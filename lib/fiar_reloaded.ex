defmodule FiarReloaded do
  @moduledoc """
  FiarReloaded keeps the contexts that define your domain
  and business logic.

  Contexts are also responsible for managing your data, regardless
  if it comes from the database, an external API or others.
  """
  defdelegate start_game(player1_name, player2_name), to: FiarReloaded.Core

  defdelegate play(game, column), to: FiarReloaded.Core
end
