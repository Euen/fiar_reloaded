defmodule FiarReloaded.GameServer do
  alias FiarReloaded.{GamesRegistry, Core, PubSub}
  alias FiarReloaded.Repo.Schemas.{Game}

  @type t() :: %__MODULE__{
          :game => Game.t(),
          :game_topic => String.t()
        }
  @enforce_keys [:game]
  defstruct [:game, :game_topic]

  @spec start_link(String.t(), String.t()) :: {:ok, pid}
  def start_link(p1_name, p2_name) do
    game_id = rand_str()
    GenServer.start_link(__MODULE__, [p1_name, p2_name], name: via_tuple(game_id))
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

  # TODO: move this to other module
  defp rand_str() do
    min = String.to_integer("100000", 36)
    max = String.to_integer("ZZZZZZ", 36)

    max
    |> Kernel.-(min)
    |> :rand.uniform()
    |> Kernel.+(min)
    |> Integer.to_string(36)
  end

  # Callbacks
  def init([p1_name, p2_name]) do
    game = Core.start_game(p1_name, p2_name)
    {:ok, %__MODULE__{:game => game}, {:continue, :notify_new_game_to_players}}
  end

  def handle_continue(:notify_new_game_to_players, %{:game => game} = state) do
    game_topic = "game_topic:#{rand_str()}"
    [game_id] = Registry.keys(GamesRegistry, self())
    payload = %{game: game, game_topic: game_topic, game_id: game_id}

    for player_id <- [game.player1.id, game.player2.id] do
      IO.inspect("user_topic:#{player_id}", label: "USER TOPIC")
      player_topic = "user_topic:#{player_id}"
      Phoenix.PubSub.broadcast(PubSub, player_topic, %{event: "game_started", payload: payload})
    end

    {:noreply, %{state | game_topic: game_topic}}
  end

  def handle_call({:play, col_number}, _from, state) do
    case Core.play(state.game, col_number) do
      {:error, reason} ->
        {:reply, {:error, reason}, state}

      {result, game} ->
        Phoenix.PubSub.broadcast(PubSub, state.game_topic, %{
          event: "chip_dropped",
          payload: %{:game => game, :result => result}
        })

        {:reply, :ok, %{state | game: game}}
    end
  end

  def handle_call({:is_user_turn, user}, _from, %{:game => game} = state) do
    result = Core.is_user_turn(game, user)
    {:reply, result, state}
  end

  def handle_call(:leave_game, _from, state) do
    Phoenix.PubSub.broadcast(PubSub, state.game_topic, %{event: "game_finished"})
    {:stop, :normal, state}
  end
end
