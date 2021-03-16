defmodule FiarReloaded do
  alias FiarReloaded.{GameSupervisor, GameServer}
  def start_game(p1_name, p2_name) do
    GameSupervisor.start_child(p1_name, p2_name)
  end

  def leave_game(game_id) do
    GameServer.leave_game(game_id)
  end

  defdelegate play(game_id, col_number), to: FiarReloaded.GameServer

  defdelegate is_user_turn(game_id, user), to: FiarReloaded.GameServer
end
