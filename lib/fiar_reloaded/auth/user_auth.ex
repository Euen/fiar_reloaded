defmodule FiarReloaded.Auth.UserAuth do
  alias FiarReloaded.Repo.Users
  alias FiarReloaded.Auth.Hash
  alias FiarReloadedWeb.Auth.Guardian

  @spec authenticate(String.t(), String.t()) :: {:ok, User.t()} | {:error, Atom.t()}
  def authenticate(username, plain_text_password) do
    with {:ok, user} <- Users.get_by_username(username),
         true <- Hash.is_valid_pass?(user.password, plain_text_password) do
      {:ok, user}
    else
      _ ->
        Argon2.no_user_verify()
        {:error, :invalid_credentials}
    end
  end

  @spec login(Plug.Conn.t(), String.t(), String.t()) ::
          {:ok, Plug.Conn.t(), User.t()} | {:error, :invalid_credentials, Plug.Conn.t()}
  def login(conn, username, password) do
    case authenticate(username, password) do
      {:ok, user} ->
        ttl = Application.get_env(:fiar_reloaded, Guardian)[:ttl]
        conn = Guardian.Plug.sign_in(conn, user, %{}, ttl: ttl)

        {:ok, conn, user}

      {:error, :invalid_credentials} ->
        {:error, :invalid_credentials, conn}
    end
  end
end
