# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
#
# This configuration file is loaded before any dependency and
# is restricted to this project.

# General application configuration
use Mix.Config

config :fiar_reloaded,
  ecto_repos: [FiarReloaded.Repo]

# Configures the endpoint
config :fiar_reloaded, FiarReloadedWeb.Endpoint,
  url: [host: "localhost"],
  secret_key_base: "kM+I5bEqT1QC6JKHKuoXfhLGLBZba1YUpgzp2UsgGR7fqgnEWtJM9bzeZeJ7p0r3",
  render_errors: [view: FiarReloadedWeb.ErrorView, accepts: ~w(html json), layout: false],
  pubsub_server: FiarReloaded.PubSub,
  live_view: [signing_salt: "T17VERtb"]

# Configures Elixir's Logger
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

# Use Jason for JSON parsing in Phoenix
config :phoenix, :json_library, Jason

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{Mix.env()}.exs"
