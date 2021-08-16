defmodule FiarReloaded.GameServer do
  alias FiarReloaded.{GamesRegistry, Core, PubSub}
  alias FiarReloaded.Repo.Schemas.{Game}

  @type t() :: %__MODULE__{
          :game => Game.t(),
          :game_topic => String.t()
        }
  @enforce_keys [:game]
  defstruct [:game, :game_topic]

  @presence "fiar_reloaded:presence"

  @spec start_link(String.t(), String.t()) :: {:ok, pid}
  def start_link(p1_name, p2_name) do
    game_id = game_id(p1_name, p2_name)
    name = via_tuple(game_id, p1_name, p2_name)
    GenServer.start_link(__MODULE__, [p1_name, p2_name], name: name)
  end

  def play(game_id, col_number) do
    call(game_id, {:play, col_number})
  end

  def is_user_turn(game_id, user) do
    call(game_id, {:is_user_turn, user})
  end

  def leave_game(game_id) do
    # TODO: transform to cast
    call(game_id, :leave_game)
    :ok
  end

  # Private functions

  defp call(game_id, action) do
    try do
      GenServer.call(via_tuple(game_id), action)
    catch
      _, _ -> {:error, :game_not_found}
    end
  end

  defp via_tuple(game_id) do
    {:via, Registry, {GamesRegistry, game_id}}
  end

  defp via_tuple(game_id, p1_name, p2_name) do
    {:via, Registry, {GamesRegistry, game_id, {p1_name, p2_name}}}
  end

  defp game_id(p1_name, p2_name) do
    [p1_name, p2_name]
    |> Enum.map(&Base.encode64/1)
    |> Enum.join()
  end

  # Callbacks
  def init(players) do
    {:ok, players, {:continue, :notify_new_game_to_players}}
  end

  def handle_continue(:notify_new_game_to_players, [p1_name, p2_name]) do
    game = Core.start_game(p1_name, p2_name)
    game_topic = "game_topic:#{game_id(p1_name, p2_name)}"

    :ok = notify_game_players(game_topic, "game_started", game)
    :ok = notify_all_players("new_game", [p1_name, p2_name])

    {:noreply, %__MODULE__{:game => game, game_topic: game_topic}}
  end

  def handle_call({:play, col_number}, _from, state) do
    case Core.play(state.game, col_number) do
      {:error, reason} ->
        {:reply, {:error, reason}, state}

      {result, game} ->
        player_number = Core.get_other_player_number(game.next_chip)

        Phoenix.PubSub.broadcast(PubSub, state.game_topic, %{
          event: "chip_dropped",
          payload: %{:game => game, :result => result, player_number: player_number}
        })

        {:reply, :ok, %{state | game: game}}
    end
  end

  def handle_call({:is_user_turn, user}, _from, %{:game => game} = state) do
    result = Core.is_user_turn(game, user)
    {:reply, result, state}
  end

  def handle_call(:leave_game, _from, %{game: %{player1: p1, player2: p2}} = state) do
    :ok = notify_game_players(state.game_topic, "game_finished")
    :ok = notify_all_players("release_players", [p1.username, p2.username])
    {:stop, :normal, state}
  end

  defp notify_game_players(game_topic, "game_finished") do
    Phoenix.PubSub.broadcast(PubSub, game_topic, %{event: "game_finished"})
  end

  defp notify_game_players(game_topic, "game_started", game) do
    [game_id] = Registry.keys(GamesRegistry, self())

    # TODO: prepare the payload with just the needed data
    payload = %{game: game, game_topic: game_topic, game_id: game_id}

    for player_id <- [game.player1.id, game.player2.id] do
      player_topic = "user_topic:#{player_id}"
      Phoenix.PubSub.broadcast(PubSub, player_topic, %{event: "game_started", payload: payload})
    end

    :ok
  end

  defp notify_all_players(event_type, payload) do
    Phoenix.PubSub.broadcast(PubSub, @presence, %{event: event_type, payload: payload})
  end
end
