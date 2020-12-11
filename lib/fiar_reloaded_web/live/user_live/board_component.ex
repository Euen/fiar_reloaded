defmodule FiarReloadedWeb.UserLive.BoardComponent do
  use FiarReloadedWeb, :live_component

  alias FiarReloaded.PubSub
  @presence "fiar_reloaded:presence"

  @impl true
  def update(%{board: board} = assigns, socket) do
    html_board = fill_with_empty(board)

    {:ok,
     socket
     |> assign(assigns)
     |> assign(:board, html_board)}
  end

  @impl true
  def handle_event("drop_chip", _params, %{assigns: %{game: nil}} = socket),
    do: {:noreply, socket}

  @impl true
  def handle_event(
        "drop_chip",
        %{"column" => col_num},
        %{assigns: %{current_user: current_user, game: game}} = socket
      ) do

    if FiarReloaded.is_user_turn(game, current_user) do
      col_num = String.at(col_num, 1)
      {_result, game} = FiarReloaded.play(game, String.to_integer(col_num))

      Phoenix.PubSub.broadcast(PubSub, @presence, %{event: "chip_dropped", payload: game})
    end

    {:noreply, socket}
  end

  defp fill_with_empty(nil), do: nil

  defp fill_with_empty(board) do
    for c <- board do
      empty_length = case 6 - length(c) do 0 -> []; l -> 1..l end
      empty_slots = for _ <- empty_length, do: :empty
      empty_slots ++ c
    end
  end
end
