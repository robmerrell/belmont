defmodule Belmont.CPU.JMP do
  @moduledoc """
  Sets the program counter to the address specified by the operand.
  """

  alias Belmont.CPU.AddressingMode

  @spec absolute(Belmont.CPU.t()) :: Belmont.CPU.t()
  def absolute(cpu) do
    address = AddressingMode.get_address(:absolute, cpu)
    %{cpu | program_counter: address.address, cycle_count: cpu.cycle_count + 3}
  end
end
