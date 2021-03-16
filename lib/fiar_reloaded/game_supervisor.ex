defmodule FiarReloaded.GameSupervisor do
  use DynamicSupervisor
  alias FiarReloaded.GameServer

  def start_link(opts) do
    DynamicSupervisor.start_link(__MODULE__, [], opts)
  end

  def init(_arg) do
    DynamicSupervisor.init(strategy: :one_for_one)
  end

  def start_child(p1_name, p2_name) do
    DynamicSupervisor.start_child(
      __MODULE__,
      %{id: GameServer, start: {GameServer, :start_link, [p1_name, p2_name]}, restart: :transient}
      )
  end
end
