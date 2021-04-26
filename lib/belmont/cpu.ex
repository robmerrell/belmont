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
    # IO.puts("step")
    Memory.read_byte(cpu.memory, cpu.program_counter)

    cpu
  end
end
