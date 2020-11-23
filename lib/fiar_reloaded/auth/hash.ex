defmodule FiarReloaded.Auth.Hash do
  @spec hash_pass(String.t()) :: String.t()
  def hash_pass(password), do: Argon2.hash_pwd_salt(password)

  @spec is_valid_pass?(String.t(), String.t()) :: boolean()
  def is_valid_pass?(hash, plain), do: Argon2.verify_pass(plain, hash)
end
