defmodule FiarReloaded.Repo do
  use Ecto.Repo,
    otp_app: :fiar_reloaded,
    adapter: Ecto.Adapters.Postgres
end
