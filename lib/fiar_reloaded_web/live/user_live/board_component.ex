defmodule FiarReloadedWeb.UserLive.BoardComponent do
  use FiarReloadedWeb, :live_component

  @impl true
  def handle_event("drop_chip", _params, %{assigns: %{game: nil}} = socket),
    do: {:noreply, socket}

  def handle_event("drop_chip", _params, %{assigns: %{result: result}} = socket)
      when result in [:won, :drawn],
      do: {:noreply, socket}

  @impl true
  def handle_event(
        "drop_chip",
        %{"column" => col_num},
        %{assigns: %{current_user: current_user, game_id: game_id}} = socket
      ) do
    socket = clear_flash(socket)

    socket =
      with true <- FiarReloaded.is_user_turn(game_id, current_user),
           col_num = String.at(col_num, 1),
           :ok <- FiarReloaded.play(game_id, String.to_integer(col_num)) do
        socket
      else
        false -> put_flash(socket, :error, "Not your turn")
        {:error, reason} -> put_flash(socket, :error, "Error: #{reason}")
      end

    {:noreply, push_patch(socket, to: "/")}
  end

end
