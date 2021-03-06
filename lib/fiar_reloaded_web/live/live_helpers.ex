defmodule FiarReloadedWeb.LiveHelpers do
  import Phoenix.LiveView.Helpers

  @doc """
  Renders a component inside the `FiarReloadedWeb.ModalComponent` component.

  The rendered modal receives a `:return_to` option to properly update
  the URL when the modal is closed.

  ## Examples

      <%= live_modal @socket, FiarReloadedWeb.UserLive.FormComponent,
        id: @user.id || :new,
        action: @live_action,
        user: @user,
        return_to: Routes.user_index_path(@socket, :index) %>
  """
  def live_modal(socket, component, opts) do
    path = Keyword.fetch!(opts, :return_to)
    modal_opts = [id: :modal, return_to: path, component: component, opts: opts]
    live_component(socket, FiarReloadedWeb.ModalComponent, modal_opts)
  end

  def get_player_class(nil), do: ""
  def get_player_class(player_number), do: "p#{player_number}"

  def get_player_number(%{:player1 => %{username: username}}, %{:username => username}), do: 1
  def get_player_number(%{:player2 => %{username: username}}, %{:username => username}), do: 2
  def get_player_number(nil, _), do: nil

  def get_result(%{:result => :next}, _), do: :next
  def get_result(%{:result => :drawn}, _), do: :drawn
  def get_result(game, current_user) do
    if game.next_chip == get_player_number(game, current_user), do: :loose, else: :won
  end

  def in_game_with_user?(username, %{:player1 => %{:username => username}}), do: true
  def in_game_with_user?(username, %{:player2 => %{:username => username}}), do: true
  def in_game_with_user?(_, _), do: false
  # def get_class(nil), do: ""
  # def get_class(class), do: class
end
