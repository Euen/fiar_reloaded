defmodule FiarReloadedWeb.SessionController do
  use FiarReloadedWeb, :controller

  alias FiarReloadedWeb.Auth.Guardian
  alias FiarReloaded.Auth.UserAuth
  alias FiarReloaded.Repo.{Users, Schemas.User}

  def new(conn, _params) do
    if Guardian.Plug.current_resource(conn) do
      redirect(conn, to: Routes.user_index_path(conn, :index))
    else
      changeset = Users.change_user(%User{})
      render(conn, "new.html", changeset: changeset, action: Routes.session_path(conn, :create))
    end
  end

  def create(conn, %{"user" => %{"username" => username, "password" => password}}) do
    case UserAuth.login(conn, username, password) do
      {:ok, conn} ->
        conn
        |> redirect(to: Routes.user_index_path(conn, :index))

      {:error, :invalid_credentials, conn} ->
        conn
        |> put_flash(:error, "Invalid username/password")
        |> redirect(to: Routes.session_path(conn, :new))
    end
  end

  def delete(conn, _) do
    conn
    |> Guardian.Plug.sign_out()
    |> redirect(to: Routes.user_index_path(conn, :index))
  end
end
