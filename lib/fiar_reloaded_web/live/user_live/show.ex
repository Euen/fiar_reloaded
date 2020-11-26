defmodule FiarReloadedWeb.UserLive.Show do
  use FiarReloadedWeb, :live_view

  alias FiarReloaded.Repo.Users

  @impl true
  def mount(_params, %{"user_id" => user_id}, socket) do
    {:ok, assign_new(socket, :current_user, fn -> Users.get_user!(user_id) end)}
  end

  @impl true
  def handle_params(%{"id" => id}, _, socket) do
    {:noreply,
     socket
     |> assign(:page_title, page_title(socket.assigns.live_action))
     |> assign(:user, Users.get_user!(id))}
  end

  defp page_title(:show), do: "Show User"
  defp page_title(:edit), do: "Edit User"
end
