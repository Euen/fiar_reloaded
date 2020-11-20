defmodule FiarReloaded.Repo.Schemas.Player do
  @type t() :: %__MODULE__{:name => String.t()}

  @enforce_keys [:name]
  defstruct [:name]
end
