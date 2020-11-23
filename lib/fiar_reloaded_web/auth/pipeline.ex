defmodule FiarReloadedWeb.Auth.Pipeline do
    @moduledoc false

    use Guardian.Plug.Pipeline,
      otp_app: :fiar_reloaded,
      module: FiarReloadedWeb.Auth.Guardian,
      error_handler: FiarReloadedWeb.Auth.ErrorHandler

    plug(Guardian.Plug.VerifySession, claims: %{"typ" => "access"})
    plug(Guardian.Plug.VerifyHeader, claims: %{"typ" => "access"})
    plug(Guardian.Plug.LoadResource, allow_blank: true)
end
