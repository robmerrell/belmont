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
            stack_pointer: 0x00,
            registers: %{a: 0x00, x: 0x00, y: 0x00, p: 0x00},
            cycle_count: 0,
            memory: %Belmont.Memory{}

  # Defines all of the opcodes supported by the CPU
  @opcodes %{
    # JMP
    0x4C => %{mnemonic: "JMP", operand_size: :word, executor: &Belmont.CPU.JMP.absolute/1}
  }

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
  Process the current instruction pointed at by the program counter.
  """
  @spec step(t()) :: t()
  def step(cpu) do
    opcode = Memory.read_byte(cpu.memory, cpu.program_counter)
    opcode_def = @opcodes[opcode]

    if opcode_def == nil,
      do: raise("Undefined opcode: #{Hexstr.hex(opcode, 2)} at #{Hexstr.hex(cpu.program_counter, 4)}")

    log(cpu, opcode, opcode_def)

    opcode_def[:executor].(cpu)
    |> step()
  end

  @doc """
  Logs an instruction and the current state of the CPU using a format that can be compared
  against output from nestest logs. We aren't logging the full mnemonic of the instruction,
  because it isn't needed.
  """
  def log(cpu, opcode, opcode_def) do
    pc = Hexstr.hex(cpu.program_counter, 4)
    op = Hexstr.hex(opcode, 2)
    stack_pointer = Hexstr.hex(cpu.stack_pointer, 2)

    operands =
      case opcode_def[:operand_size] do
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

    mnemonic = String.pad_trailing(opcode_def[:mnemonic], 31, " ")

    cyc =
      Integer.mod(cpu.cycle_count * 3, 341)
      |> Integer.to_string()
      |> String.pad_leading(3, " ")

    "#{pc}  #{op} #{operands} #{mnemonic} #{flags} SP:#{stack_pointer} CYC:#{cyc}"
    |> String.upcase()
    |> IO.puts()
  end
end
