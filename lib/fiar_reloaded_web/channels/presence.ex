defmodule FiarReloadedWeb.Presence do
  use Phoenix.Presence,
    otp_app: :fiar_reloaded,
    pubsub_server: FiarReloaded.PubSub
end
