defmodule FiarReloded.Repo do
  use Ecto.Repo,
    otp_app: :fiar_reloded,
    adapter: Ecto.Adapters.Postgres
end
