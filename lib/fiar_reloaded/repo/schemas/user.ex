defmodule FiarReloaded.Repo.Schemas.User do
  use Ecto.Schema
  import Ecto.Changeset
  alias FiarReloaded.Auth.Hash

  schema "users" do
    field :password, :string
    field :username, :string

    timestamps()
  end

  @doc false
  def changeset(user, attrs) do
    user
    |> cast(attrs, [:username, :password])
    |> validate_required([:username, :password])
    |> validate_length(:password, min: 6)
    |> put_password_hash()
    |> unique_constraint(:username)
  end

  defp put_password_hash(%Ecto.Changeset{valid?: true, changes: %{password: password}} = changeset) do
    change(changeset, password: Hash.hash_pass(password))
  end

  defp put_password_hash(changeset), do: changeset
end
