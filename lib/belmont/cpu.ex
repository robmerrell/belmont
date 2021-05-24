defmodule Belmont.CPU do
  @moduledoc """
  The cpu represents everything we need to emulate the NES CPU. The NES' CPU is a variation of the 6502
  processor that runs at 1.79 MHz (PAL regions is 1.66 MHz). One of the differences between the NES' CPU and the standard
  MOS6502 is that the NES does not have a decimal mode. Saving us a bit of work. The CPU is little endian.

  The NES CPU has 4 work registers (excluding program_counter and the stack_pointer). All 4 registers
  are a byte wide.

  * :a - The accumulator register.
  * :x - A general purpose index register.
  * :y - A general purpose index register.
  * :p - The status register, used to store flags.
  """

  use Bitwise
  alias Belmont.CPU.AddressingMode
  alias Belmont.Memory
  alias Belmont.Hexstr

  @typedoc """
  Defines the CPU state.
  * :program_counter - A 16-bit register that points to the current instruction to be processed by the CPU.
  * :stack_pointer - An 8-bit register that points at the *TOP* of the stack. The stack is located from $0100-01FF,
    so the stack pointer works as an offset pointing between the beginning and ending memory locations.
    The stack is top-down, so the stack pointer is decremented when a byte is pushed onto the stack.
  * :registers - CPU registers.
  * :cycle_count - Counts the number of CPU cycles used during execution. This is our primary mechanism for pacing.
  * :memory - Memory struct.
  """
  @type t :: %__MODULE__{
          program_counter: integer(),
          stack_pointer: byte(),
          registers: %{a: byte(), x: byte(), y: byte(), p: byte()},
          cycle_count: integer(),
          memory: Belmont.Memory.t()
        }

  defstruct program_counter: 0x0000,
            stack_pointer: 0xFD,
            registers: %{a: 0x00, x: 0x00, y: 0x00, p: 0x24},
            cycle_count: 0,
            memory: %Belmont.Memory{}

  # Defines all of the possible flags available to be set on the status register and the bit
  # where they are located. Bits 3-5 of the status flag are not used in the NES for flags, but
  # can be used to serve other purposes.
  @flags %{
    carry: 1 <<< 0,
    zero: 1 <<< 1,
    interrupt: 1 <<< 2,
    decimal: 1 <<< 3,
    overflow: 1 <<< 6,
    negative: 1 <<< 7
  }

  @doc """
  Creates a new CPU
  """
  @spec new(Belmont.Memory.t()) :: t()
  def new(memory) do
    %__MODULE__{memory: memory}
  end

  @doc """
  Test if a given flag is set on the flag register.
  """
  @spec flag_set?(t(), atom()) :: boolean()
  def flag_set?(cpu, flag) do
    band(cpu.registers.p, @flags[flag]) != 0
  end

  @doc """
  Set the given flag on the flag register.
  """
  @spec set_flag(t(), atom()) :: t()
  def set_flag(cpu, flag) do
    registers = %{cpu.registers | p: bor(cpu.registers.p, @flags[flag])}
    %{cpu | registers: registers}
  end

  @doc """
  Set or unset the given flag using a test byte to determine the flag's state
  """
  def set_flag_with_test(cpu, :zero, test_byte) do
    if test_byte == 0, do: set_flag(cpu, :zero), else: unset_flag(cpu, :zero)
  end

  def set_flag_with_test(cpu, :negative, test_byte) do
    if band(test_byte, 0x80) != 0, do: set_flag(cpu, :negative), else: unset_flag(cpu, :negative)
  end

  @doc """
  Unset the given flag on the status register.
  """
  @spec unset_flag(t(), atom()) :: t()
  def unset_flag(cpu, flag) do
    registers = %{cpu.registers | p: cpu.registers.p &&& bnot(@flags[flag])}
    %{cpu | registers: registers}
  end

  @doc """
  Set a register to a specific value.
  """
  @spec set_register(t(), atom(), byte()) :: t()
  def set_register(cpu, register_key, value) when value <= 0xFF and value >= 0 do
    registers = Map.put(cpu.registers, register_key, value)
    %{cpu | registers: registers}
  end

  @doc """
  Pushes a byte onto the stack.
  """
  @spec push_byte_onto_stack(t(), byte()) :: t()
  def push_byte_onto_stack(cpu, byte) do
    memory = Memory.write_byte(cpu.memory, 0x100 + cpu.stack_pointer, byte)
    # simulate byte overflow
    wrapped_stack_pointer = Integer.mod(cpu.stack_pointer - 1, 256)

    %{cpu | memory: memory, stack_pointer: wrapped_stack_pointer}
  end

  @doc """
  Pushes a word onto a stack by writing each byte individually.
  """
  @spec push_word_onto_stack(t(), integer()) :: t()
  def push_word_onto_stack(cpu, word) do
    <<high_byte, low_byte>> = <<word::size(16)>>

    cpu
    |> push_byte_onto_stack(high_byte)
    |> push_byte_onto_stack(low_byte)
  end

  @doc """
  Pops a byte off of the stack.
  """
  @spec pop_byte_off_stack(t()) :: {t(), byte()}
  def pop_byte_off_stack(cpu) do
    wrapped_stack_pointer = Integer.mod(cpu.stack_pointer + 1, 256)
    byte = Memory.read_byte(cpu.memory, 0x100 + wrapped_stack_pointer)
    {%{cpu | stack_pointer: wrapped_stack_pointer}, byte}
  end

  @doc """
  Process the current instruction pointed at by the program counter.
  """
  @spec step(t()) :: t()
  def step(cpu) do
    opcode = Memory.read_byte(cpu.memory, cpu.program_counter)
    execute(cpu, opcode)
    # |> step()
  end

  # Logs an instruction and the current state of the CPU using a format that can be compared
  # against output from nestest logs. We aren't logging the full mnemonic of the instruction,
  # because it isn't needed.
  defp log_state(cpu, opcode, mnemonic, operand_size) do
    pc = Hexstr.hex(cpu.program_counter, 4)
    op = Hexstr.hex(opcode, 2)
    stack_pointer = Hexstr.hex(cpu.stack_pointer, 2)

    operands =
      case operand_size do
        :byte ->
          Belmont.Memory.read_byte(cpu.memory, cpu.program_counter + 1) |> Hexstr.hex(2)

        :word ->
          low_byte = Belmont.Memory.read_byte(cpu.memory, cpu.program_counter + 1) |> Hexstr.hex(2)
          high_byte = Belmont.Memory.read_byte(cpu.memory, cpu.program_counter + 2) |> Hexstr.hex(2)
          "#{low_byte} #{high_byte}"

        _ ->
          ""
      end

    operands = String.pad_trailing(operands, 6, " ")

    # flags
    a = Hexstr.hex(cpu.registers[:a], 2)
    x = Hexstr.hex(cpu.registers[:x], 2)
    y = Hexstr.hex(cpu.registers[:y], 2)
    p = Hexstr.hex(cpu.registers[:p], 2)
    flags = "A:#{a} X:#{x} Y:#{y} P:#{p}"

    mnemonic = String.pad_trailing(mnemonic, 31, " ")

    cyc =
      Integer.mod(cpu.cycle_count * 3, 341)
      |> Integer.to_string()
      |> String.pad_leading(3, " ")

    "#{pc}  #{op} #{operands} #{mnemonic} #{flags} SP:#{stack_pointer} CYC:#{cyc}"
    |> String.upcase()
  end

  # instruction logging
  def log(cpu, 0x20), do: log_state(cpu, 0x20, "JSR", :word)
  def log(cpu, 0x38), do: log_state(cpu, 0x38, "SEC", :none)
  def log(cpu, 0x4C), do: log_state(cpu, 0x4C, "JMP", :word)
  def log(cpu, 0x86), do: log_state(cpu, 0x86, "STX", :byte)
  def log(cpu, 0xA2), do: log_state(cpu, 0xA2, "LDX", :byte)
  def log(cpu, 0xEA), do: log_state(cpu, 0xEA, "NOP", :none)
  def log(cpu, opcode), do: log_state(cpu, opcode, "UNDEF", :none)

  # instruction execution
  defp execute(cpu, 0x20), do: jsr(cpu, :absolute)
  defp execute(cpu, 0x38), do: set_flag_op(cpu, :carry)
  defp execute(cpu, 0x4C), do: jmp(cpu, :absolute)
  defp execute(cpu, 0x86), do: stx(cpu, :zero_page)
  defp execute(cpu, 0xA2), do: ldx(cpu, :immediate)
  defp execute(cpu, 0xEA), do: nop(cpu, :implied)

  defp execute(cpu, opcode) do
    raise("Undefined opcode: #{Hexstr.hex(opcode, 2)} at #{Hexstr.hex(cpu.program_counter, 4)}")
  end

  def nop(cpu, :implied) do
    %{cpu | program_counter: cpu.program_counter + 1, cycle_count: cpu.cycle_count + 2}
  end

  # set a flag to 1
  def set_flag_op(cpu, flag) do
    cpu
    |> set_flag(flag)
    |> Map.put(:program_counter, cpu.program_counter + 1)
    |> Map.put(:cycle_count, cpu.cycle_count + 2)
  end

  # read a word and set the program counter to that value
  def jmp(cpu, addressing_mode) do
    address = AddressingMode.get_address(addressing_mode, cpu)

    {pc, cycle} =
      case addressing_mode do
        :absolute -> {address.address, 3}
      end

    %{cpu | program_counter: pc, cycle_count: cpu.cycle_count + cycle}
  end

  # load value read at address into the :x register
  def ldx(cpu, addressing_mode) do
    byte_address = AddressingMode.get_address(addressing_mode, cpu)
    byte = Memory.read_byte(cpu.memory, byte_address.address)

    {pc, cycle} =
      case addressing_mode do
        :immediate -> {2, 2}
        :zero_page -> {2, 3}
        :zero_page_y -> {2, 4}
        :absolute -> {3, 4}
        :absolute_y -> if byte_address.page_crossed, do: {3, 4}, else: {3, 5}
      end

    cpu
    |> set_register(:x, byte)
    |> set_flag_with_test(:zero, byte)
    |> set_flag_with_test(:negative, byte)
    |> Map.put(:program_counter, cpu.program_counter + pc)
    |> Map.put(:cycle_count, cpu.cycle_count + cycle)
  end

  # stores the contents of the :x register into memory
  def stx(cpu, addressing_mode) do
    address = AddressingMode.get_address(addressing_mode, cpu)
    memory = Memory.write_byte(cpu.memory, address.address, cpu.registers.x)

    {pc, cycle} =
      case addressing_mode do
        :zero_page -> {2, 3}
        :zero_page_y -> {2, 4}
        :absolute -> {2, 4}
      end

    %{cpu | memory: memory, program_counter: cpu.program_counter + pc, cycle_count: cpu.cycle_count + cycle}
  end

  # push the address (minus one) of the return point to the stack and set the program counter to the memory address
  def jsr(cpu, addressing_mode) do
    byte_address = AddressingMode.get_address(addressing_mode, cpu)
    cpu = push_word_onto_stack(cpu, cpu.program_counter + 2)
    %{cpu | program_counter: byte_address.address, cycle_count: cpu.cycle_count + 6}
  end
end
