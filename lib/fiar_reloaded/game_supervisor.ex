defmodule FiarReloaded.GameSupervisor do
  use DynamicSupervisor
  alias FiarReloaded.{GameServer, GamesRegistry}

  def start_link(opts) do
    DynamicSupervisor.start_link(__MODULE__, [], opts)
  end

  def init(_arg) do
    DynamicSupervisor.init(strategy: :one_for_one)
  end

  def start_child(p1_name, p2_name) do
    players = get_all_players()

    if p1_name not in players and p2_name not in players do
      DynamicSupervisor.start_child(
        __MODULE__,
        %{
          id: GameServer,
          start: {GameServer, :start_link, [p1_name, p2_name]},
          restart: :transient
        }
      )
    else
      {:error, :already_in_game}
    end
  end

  def get_all_players() do
    Registry.select(GamesRegistry, [{{:_, :_, :"$1"}, [], [{{:"$1"}}]}])
    |> Enum.reduce([], fn {{n1, n2}}, acc -> [n1, n2 | acc] end)
  end
end
