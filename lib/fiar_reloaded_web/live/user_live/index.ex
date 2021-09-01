defmodule FiarReloadedWeb.UserLive.Index do
  use FiarReloadedWeb, :live_view

  alias FiarReloaded.Repo.Users
  alias FiarReloaded.Repo.Schemas.{User}

  alias FiarReloadedWeb.Presence
  alias FiarReloaded.{PubSub, GameSupervisor}
  alias FiarReloadedWeb.Router.Helpers, as: Routes

  @presence "fiar_reloaded:presence"

  @impl true
  def mount(_params, session, socket) do
    {user, players} =
      if connected?(socket) and !is_nil(session["user_id"]) do
        Phoenix.PubSub.subscribe(PubSub, @presence)

        user =
          session["user_id"]
          |> Users.get_user!()

        Phoenix.PubSub.subscribe(PubSub, "user_topic:#{user.id}")

        {:ok, _} =
          Presence.track(self(), @presence, user.id, %{
            username: user.username
          })

        # TODO: do this function  and check if is better to move this to the handle_params section
        players = GameSupervisor.get_all_players()
        {user, players}
      else
        {nil, []}
      end

    {
      :ok,
      socket
      |> assign(:current_user, user)
      |> assign(:logged_users, %{})
      |> assign(:users, list_users())
      |> assign(:players_in_game, players)
      |> handle_joins(Presence.list(@presence))
    }
  end

  @impl true
  def handle_params(params, _url, socket) do
    {:noreply, apply_action(socket, socket.assigns.live_action, params)}
  end

  defp apply_action(socket, :edit, %{"id" => id}) do
    socket
    |> assign(:page_title, "Edit User")
    |> assign(:user, Users.get_user!(id))
  end

  defp apply_action(socket, :new, _params) do
    socket
    |> assign(:page_title, "New User")
    |> assign(:user, %User{})
  end

  defp apply_action(socket, :index, _params) do
    socket
    |> assign(:page_title, "Listing Users")
    |> assign(:user, nil)
  end

  @impl true
  def handle_event("delete", %{"id" => id}, socket) do
    user = Users.get_user!(id)
    {:ok, _} = Users.delete_user(user)

    {:noreply, assign(socket, :users, list_users())}
  end

  @impl true
  def handle_event("start_game", %{"p2_name" => p2_name}, socket) do
    case get_in(socket.assigns, [:current_user, :username]) do
      nil ->
        {:noreply, push_patch(socket, to: Routes.user_index_path(socket, :new))}

      p1_name ->
        # Starts a supervised process in charge of hanlde the state of the game
        _ = FiarReloaded.start_game(p1_name, p2_name)
        {:noreply, socket}
    end
  end

  @impl true
  def handle_event("leave_game", _, socket) do
    :ok = FiarReloaded.leave_game(socket.assigns.game_id)

    {:noreply, socket}
  end

  @impl true
  def handle_info(%{event: "presence_diff", payload: diff}, socket) do
    {
      :noreply,
      socket
      |> handle_leaves(diff.leaves)
      |> handle_joins(diff.joins)
    }
  end

  # @impl true
  def handle_info(
        %{
          event: "game_started",
          payload: %{:game => game, :game_topic => game_topic, :game_id => game_id}
        },
        socket
      ) do
    Phoenix.PubSub.subscribe(PubSub, game_topic)

    socket =
      socket
      |> assign(:game, game)
      |> assign(:game_id, game_id)

    {:noreply, socket}
  end

  # @impl true
  def handle_info(
        %{
          event: "chip_dropped",
          payload: %{:game => game, :result => result, :player_number => player_number}
        },
        socket
      ) do
    last_row = game.last_row_played
    last_col = game.last_col_played

    socket =
      socket
      |> assign(:game, game)
      |> assign(:result, result)
      |> assign(:last_row, last_row)
      |> assign(:last_col, last_col)
      |> assign(:player_number, player_number)

    {:noreply, socket}
  end

  # @impl true
  def handle_info(%{event: "game_finished"}, socket) do
    socket =
      socket
      |> delete_from_assign(:game)
      |> delete_from_assign(:last_row)
      |> delete_from_assign(:last_col)
      |> delete_from_assign(:result)
      |> delete_from_assign(:game_id)

    {:noreply, socket}
  end

  # @imp true
  def handle_info(%{event: "new_game", payload: new_players}, socket) do
    {:noreply, update(socket, :players_in_game, &(new_players ++ &1))}
  end

  # @imp true
  def handle_info(%{event: "release_players", payload: ex_players}, socket) do
    {:noreply, update(socket, :players_in_game, &Enum.reject(&1, fn pl -> pl in ex_players end))}
  end

  # @imp true
  def handle_info(_, socket), do: {:noreply, socket}

  defp delete_from_assign(socket, key) do
    %{
      socket
      | assigns: Map.delete(socket.assigns, key),
        changed: Map.put_new(socket.changed, key, true)
    }
  end

  defp handle_joins(socket, joins) do
    Enum.reduce(joins, socket, fn {user, %{metas: [meta | _]}}, socket ->
      assign(socket, :logged_users, Map.put(socket.assigns.logged_users, user, meta))
    end)
  end

  defp handle_leaves(socket, leaves) do
    Enum.reduce(leaves, socket, fn {user, _}, socket ->
      assign(socket, :logged_users, Map.delete(socket.assigns.logged_users, user))
    end)
  end

  defp list_users do
    Users.list_users()
  end
end
