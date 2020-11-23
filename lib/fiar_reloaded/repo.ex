defmodule FiarReloaded.Repo do
  use Ecto.Repo,
    otp_app: :fiar_reloaded,
    adapter: Ecto.Adapters.Postgres

  alias FiarReloaded.Repo

  @type error_reason() ::
          {:not_found, binary()}
          | {:multiple_Results, atom()}
          | {:invalid_argument_cast_error, atom()}

  @type error_tuple() :: {:error, error_reason()}

  @type success_tuple() :: {:ok, Ecto.Schema.t()}

  @type result_tuple() :: success_tuple() | error_tuple()

  @spec tupled_get(Ecto.Schema.t(), pos_integer()) :: result_tuple()
  def tupled_get(schema, id) when is_nil(id) or id == "" do
    {:error, {:not_found, Atom.to_string(schema)}}
  end

  def tupled_get(schema, id) do
    args = Map.put(%{}, :id, id)
    tupled_get_by(schema, args)
  end

  @spec tupled_get_by(Ecto.Queryable.t(), Keyword.t() | map()) :: result_tuple()
  def tupled_get_by(schema, search_terms) do
    result = Repo.get_by(schema, search_terms)

    case result do
      nil -> {:error, {:not_found, Atom.to_string(schema)}}
      result -> {:ok, result}
    end
  rescue
    _ in Ecto.MultipleResultsError ->
      {:error, {:multiple_results, schema}}

    _ in Ecto.Query.CastError ->
      {:error, {:invalid_argument_cast_error, schema}}
  end
end
