defmodule Belmont do
  @moduledoc """
  Entry point for the emluator
  """

  def main(_args \\ []) do
    Belmont.Cartridge.load_rom("")
    |> Belmont.Memory.new()
    |> Belmont.CPU.new()
    |> Belmont.CPU.step()
  end
end
