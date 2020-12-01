defmodule FiarReloadedWeb.UserLive.Index do
  use FiarReloadedWeb, :live_view

  alias FiarReloaded.Repo.Users
  alias FiarReloaded.Repo.Schemas.User

  alias FiarReloadedWeb.Presence
  alias FiarReloaded.PubSub

  @presence "fiar_reloaded:presence"

  @impl true
  def mount(_params, session, socket) do
    user =
      if connected?(socket) and !is_nil(session["user_id"]) do
        Phoenix.PubSub.subscribe(PubSub, @presence)

        user =
          session["user_id"]
          |> Users.get_user!()

        {:ok, _} =
          Presence.track(self(), @presence, user.id, %{
            username: user.username
          })
        user
      else
        nil
      end

    {
      :ok,
      socket
      |> assign(:current_user, user)
      |> assign(:logged_users, %{})
      |> assign(:users, list_users())
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
  def handle_info(%{event: "presence_diff", payload: diff}, socket) do
    {
      :noreply,
      socket
      |> handle_leaves(diff.leaves)
      |> handle_joins(diff.joins)
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
