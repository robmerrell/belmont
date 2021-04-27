defmodule Belmont.Hexstr do
  @moduledoc """
  Module for handling hex strings
  """

  @doc """
  Converts an integer to a hex string
  """
  @spec hex(integer(), integer()) :: String.t()
  def hex(number, padding \\ 2) do
    number
    |> Integer.to_string(16)
    |> String.pad_leading(padding, "0")
  end
end
