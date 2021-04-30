defmodule Belmont.CPU.AddressingMode do
  @doc """
  Handles the various addressing modes of the CPU. Also determines per mode if a page boundry
  was crossed so that the extra cycles can be accounted for in the CPU.
  """

  use Bitwise
  alias Belmont.Memory

  defstruct address: 0x0000, page_crossed: false, additional_cycles: 0

  @doc """
  Get a memory address using the given addressing mode. Accepted modes are:
  * zero_page
  * zero_page_x
  * zero_page_y
  * absolute
  * absolute_x
  * absolute_y
  * indirect
  * indexed_indirect
  * immediate
  * implied
  * accumulator
  * relative
  """
  # modes that don't return addresses
  def get_address(:implied, _cpu_state),
    do: %__MODULE__{address: 0, page_crossed: false, additional_cycles: 0}

  def get_address(:accumulator, _cpu_state),
    do: %__MODULE__{address: 0, page_crossed: false, additional_cycles: 0}

  # zero page addresses. The zero page exists at 0x0000-0x00ff so only one byte is needed.
  def get_address(:zero_page, cpu_state), do: zero_page_address(cpu_state, 0)
  def get_address(:zero_page_x, cpu_state), do: zero_page_address(cpu_state, cpu_state.registers.x)
  def get_address(:zero_page_y, cpu_state), do: zero_page_address(cpu_state, cpu_state.registers.y)

  # absolute addresses. Address obtained from the 2 current operands.
  def get_address(:absolute, cpu_state), do: absolute_address(cpu_state, 0)
  def get_address(:absolute_x, cpu_state), do: absolute_address(cpu_state, cpu_state.registers.x)
  def get_address(:absolute_y, cpu_state), do: absolute_address(cpu_state, cpu_state.registers.y)

  # indirect addresses.
  def get_address(:indirect, cpu_state), do: indirect_address(cpu_state, 0)
  def get_address(:indexed_indirect, cpu_state), do: indirect_address(cpu_state, cpu_state.registers.x)

  # immediate address
  def get_address(:immediate, cpu_state),
    do: %__MODULE__{address: cpu_state.program_counter + 1, page_crossed: false, additional_cycles: 0}

  # relative address.
  def get_address(:relative, cpu_state) do
    branch_offset = Memory.read_byte(cpu_state.memory, cpu_state.program_counter + 1)

    # since the branch offset needs to be interpreted as a signed byte we treat the values 0x00-0x7f as positive
    # and the values 0x80-0xff as negative.
    address =
      if branch_offset < 0x80 do
        cpu_state.program_counter + 2 + branch_offset
      else
        cpu_state.program_counter + 2 + branch_offset - 0x100
      end

    cycles = if page_crossed?(cpu_state.program_counter, address), do: 2, else: 1
    %__MODULE__{address: address, page_crossed: false, additional_cycles: cycles}
  end

  # handles zero page addresses
  defp zero_page_address(cpu_state, addend) do
    address = Memory.read_byte(cpu_state.memory, cpu_state.program_counter + 1)
    # simulate byte overflow
    wrapped = rem(address + addend, 256)
    %__MODULE__{address: wrapped, page_crossed: false, additional_cycles: 0}
  end

  defp absolute_address(cpu_state, addend) do
    address = Memory.read_word(cpu_state.memory, cpu_state.program_counter + 1)

    %__MODULE__{
      address: address + addend,
      page_crossed: page_crossed?(address, address + addend),
      additional_cycles: 0
    }
  end

  defp indirect_address(cpu_state, addend) do
    address = Memory.read_word(cpu_state.memory, cpu_state.program_counter + 1) + addend

    low_byte = Memory.read_byte(cpu_state.memory, address)
    high_byte = Memory.read_byte(cpu_state.memory, address + 1)

    %__MODULE__{address: high_byte <<< 8 ||| low_byte, page_crossed: false, additional_cycles: 0}
  end

  # determine if a page cross occured between two addresses
  defp page_crossed?(address1, address2) do
    Bitwise.band(address1, 0xFF00) != Bitwise.band(address2, 0xFF00)
  end
end
