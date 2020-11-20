defmodule FiarReloaded.Repo.Players do
  alias FiarReloaded.Repo.Schemas.Player

  @spec new(String.t()) :: Player.t()
  def new(player_name) do
    %Player{name: player_name}
  end
end
