defmodule FiarReloadedWeb.ViewHelper do
  def current_user(conn), do: Guardian.Plug.current_resource(conn)

  def logged_in?(conn), do: Guardian.Plug.authenticated?(conn)

  def is_logged_user?(user, current_user), do: user == current_user
end
