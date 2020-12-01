defmodule FiarReloaded.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  def start(_type, _args) do
    children = [
      # Start the Ecto repository
      FiarReloaded.Repo,
      # Start the Telemetry supervisor
      FiarReloadedWeb.Telemetry,
      # Start the PubSub system
      {Phoenix.PubSub, name: FiarReloaded.PubSub},
      # Start Presence
      FiarReloadedWeb.Presence,
      # Start the Endpoint (http/https)
      FiarReloadedWeb.Endpoint
      # Start a worker by calling: FiarReloaded.Worker.start_link(arg)
      # {FiarReloaded.Worker, arg}
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: FiarReloaded.Supervisor]
    Supervisor.start_link(children, opts)
  end

  # Tell Phoenix to update the endpoint configuration
  # whenever the application is updated.
  def config_change(changed, _new, removed) do
    FiarReloadedWeb.Endpoint.config_change(changed, removed)
    :ok
  end
end
