defmodule FiarReloadedWeb.Auth.ErrorHandler do
  import Plug.Conn
  alias FiarReloadedWeb.Router.Helpers, as: Routes

  @behaviour Guardian.Plug.ErrorHandler

  @impl Guardian.Plug.ErrorHandler
  def auth_error(conn, {_type, _reason}, _opts) do
    conn
    |> clear_session()
    |> Phoenix.Controller.redirect(to: Routes.session_path(conn, :new))
  end
end
