defmodule Belmont.CPU do
  @moduledoc """
  The cpu represents everything we need to emulate the NES CPU. The NES' CPU is a variation of the 6502
  processor that runs at 1.79 MHz (PAL regions is 1.66 MHz). One of the differences between the NES' CPU and the standard
  MOS6502 is that the NES does not have a decimal mode, saving us a bit of work. The CPU is little endian.

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
    unused_1: 1 <<< 4,
    unused_2: 1 <<< 5,
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

  def set_flag_with_test(cpu, :overflow, test_byte) do
    if band(test_byte, 0x70) != 0, do: set_flag(cpu, :overflow), else: unset_flag(cpu, :overflow)
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
  def log(cpu, 0x08), do: log_state(cpu, 0x08, "PHP", :none)
  def log(cpu, 0x10), do: log_state(cpu, 0x10, "BPL", :byte)
  def log(cpu, 0x18), do: log_state(cpu, 0x18, "CLC", :none)
  def log(cpu, 0x20), do: log_state(cpu, 0x20, "JSR", :word)
  def log(cpu, 0x24), do: log_state(cpu, 0x24, "BIT", :byte)
  def log(cpu, 0x28), do: log_state(cpu, 0x28, "PLP", :none)
  def log(cpu, 0x29), do: log_state(cpu, 0x29, "AND", :byte)
  def log(cpu, 0x2C), do: log_state(cpu, 0x2C, "BIT", :word)
  def log(cpu, 0x38), do: log_state(cpu, 0x38, "SEC", :none)
  def log(cpu, 0x48), do: log_state(cpu, 0x48, "PHA", :none)
  def log(cpu, 0x4C), do: log_state(cpu, 0x4C, "JMP", :word)
  def log(cpu, 0x50), do: log_state(cpu, 0x50, "BVC", :byte)
  def log(cpu, 0x60), do: log_state(cpu, 0x60, "RTS", :none)
  def log(cpu, 0x68), do: log_state(cpu, 0x68, "PLA", :none)
  def log(cpu, 0x70), do: log_state(cpu, 0x70, "BVS", :byte)
  def log(cpu, 0x78), do: log_state(cpu, 0x78, "SEI", :none)
  def log(cpu, 0x85), do: log_state(cpu, 0x85, "STA", :byte)
  def log(cpu, 0x86), do: log_state(cpu, 0x86, "STX", :byte)
  def log(cpu, 0x90), do: log_state(cpu, 0x90, "BCC", :byte)
  def log(cpu, 0xA2), do: log_state(cpu, 0xA2, "LDX", :byte)
  def log(cpu, 0xA9), do: log_state(cpu, 0xA9, "LDA", :byte)
  def log(cpu, 0xB0), do: log_state(cpu, 0xB0, "BCS", :byte)
  def log(cpu, 0xC9), do: log_state(cpu, 0xC9, "CMP", :byte)
  def log(cpu, 0xD0), do: log_state(cpu, 0xD0, "BNE", :byte)
  def log(cpu, 0xD8), do: log_state(cpu, 0xD8, "CLD", :none)
  def log(cpu, 0xEA), do: log_state(cpu, 0xEA, "NOP", :none)
  def log(cpu, 0xF0), do: log_state(cpu, 0xF0, "BEQ", :byte)
  def log(cpu, 0xF8), do: log_state(cpu, 0xF8, "SED", :none)
  def log(cpu, opcode), do: log_state(cpu, opcode, "UNDEF", :none)

  # instruction execution
  defp execute(cpu, 0x08), do: php(cpu)
  defp execute(cpu, 0x10), do: branch_if(cpu, fn cpu -> !flag_set?(cpu, :negative) end)
  defp execute(cpu, 0x18), do: unset_flag_op(cpu, :carry)
  defp execute(cpu, 0x20), do: jsr(cpu, :absolute)
  defp execute(cpu, 0x24), do: bit(cpu, :zero_page)
  defp execute(cpu, 0x28), do: plp(cpu)
  defp execute(cpu, 0x29), do: and_instr(cpu, :immediate)
  defp execute(cpu, 0x2C), do: bit(cpu, :absolute)
  defp execute(cpu, 0x38), do: set_flag_op(cpu, :carry)
  defp execute(cpu, 0x48), do: pha(cpu)
  defp execute(cpu, 0x4C), do: jmp(cpu, :absolute)
  defp execute(cpu, 0x50), do: branch_if(cpu, fn cpu -> !flag_set?(cpu, :overflow) end)
  defp execute(cpu, 0x60), do: rts(cpu)
  defp execute(cpu, 0x68), do: pla(cpu)
  defp execute(cpu, 0x70), do: branch_if(cpu, fn cpu -> flag_set?(cpu, :overflow) end)
  defp execute(cpu, 0x78), do: set_flag_op(cpu, :interrupt)
  defp execute(cpu, 0x85), do: sta(cpu, :zero_page)
  defp execute(cpu, 0x86), do: stx(cpu, :zero_page)
  defp execute(cpu, 0x90), do: branch_if(cpu, fn cpu -> !flag_set?(cpu, :carry) end)
  defp execute(cpu, 0xA2), do: ldx(cpu, :immediate)
  defp execute(cpu, 0xA9), do: lda(cpu, :immediate)
  defp execute(cpu, 0xB0), do: branch_if(cpu, fn cpu -> flag_set?(cpu, :carry) end)
  defp execute(cpu, 0xC9), do: cmp(cpu, :immediate)
  defp execute(cpu, 0xD0), do: branch_if(cpu, fn cpu -> !flag_set?(cpu, :zero) end)
  defp execute(cpu, 0xD8), do: unset_flag_op(cpu, :decimal)
  defp execute(cpu, 0xEA), do: nop(cpu, :implied)
  defp execute(cpu, 0xF0), do: branch_if(cpu, fn cpu -> flag_set?(cpu, :zero) end)
  defp execute(cpu, 0xF8), do: set_flag_op(cpu, :decimal)

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

  # set a flag to 0
  def unset_flag_op(cpu, flag) do
    cpu
    |> unset_flag(flag)
    |> Map.put(:program_counter, cpu.program_counter + 1)
    |> Map.put(:cycle_count, cpu.cycle_count + 2)
  end

  # ands the byte in memory with the accumulator
  def and_instr(cpu, addressing_mode) do
    byte_address = AddressingMode.get_address(addressing_mode, cpu)
    byte = Memory.read_byte(cpu.memory, byte_address.address)
    anded = cpu.registers.a &&& byte

    {pc, cycle} =
      case addressing_mode do
        :immediate -> {2, 2}
        :zero_page -> {2, 3}
        :zero_page_x -> {2, 4}
        :absolute -> {3, 4}
        :absolute_x -> if byte_address.page_crossed, do: {3, 4}, else: {3, 5}
        :absolute_y -> if byte_address.page_crossed, do: {3, 4}, else: {3, 5}
        :indexed_indirect -> {2, 6}
        :indirect_indexed -> if byte_address.page_crossed, do: {2, 5}, else: {2, 6}
      end

    cpu
    |> set_register(:a, anded)
    |> set_flag_with_test(:zero, anded)
    |> set_flag_with_test(:negative, anded)
    |> Map.put(:program_counter, cpu.program_counter + pc)
    |> Map.put(:cycle_count, cpu.cycle_count + cycle)
  end

  # read a word and set the program counter to that value
  def jmp(cpu, addressing_mode) do
    address = AddressingMode.get_address(addressing_mode, cpu)

    {pc, cycles} =
      case addressing_mode do
        :absolute -> {address.address, 3}
      end

    %{cpu | program_counter: pc, cycle_count: cpu.cycle_count + cycles}
  end

  # test if one or more bits are set
  def bit(cpu, addressing_mode) do
    byte_address = AddressingMode.get_address(addressing_mode, cpu)
    byte = Memory.read_byte(cpu.memory, byte_address.address)
    res = cpu.registers.a &&& byte

    {pc, cycle} =
      case addressing_mode do
        :zero_page -> {2, 3}
        :absolute -> {3, 4}
      end

    cpu
    |> set_flag_with_test(:zero, res)
    |> set_flag_with_test(:negative, res)
    |> set_flag_with_test(:overflow, res)
    |> Map.put(:program_counter, cpu.program_counter + pc)
    |> Map.put(:cycle_count, cpu.cycle_count + cycle)
  end

  # compare accumulator with a byte from memory and sets flags
  def cmp(cpu, addressing_mode) do
    byte_address = AddressingMode.get_address(addressing_mode, cpu)
    byte = Memory.read_byte(cpu.memory, byte_address.address)

    {pc, cycle} =
      case addressing_mode do
        :immediate -> {2, 2}
        :zero_page -> {2, 3}
        :zero_page_x -> {2, 4}
        :absolute -> {3, 4}
        :absolute_x -> if byte_address.page_crossed, do: {3, 4}, else: {3, 5}
        :absolute_y -> if byte_address.page_crossed, do: {3, 4}, else: {3, 5}
        :indexed_indirect -> {2, 6}
        :indirect_indexed -> if byte_address.page_crossed, do: {2, 5}, else: {2, 6}
      end

    cpu = if cpu.registers.a >= byte, do: set_flag(cpu, :carry), else: cpu
    cpu = if cpu.registers.a == byte, do: set_flag(cpu, :zero), else: cpu

    cpu
    |> set_flag_with_test(:negative, byte)
    |> Map.put(:program_counter, cpu.program_counter + pc)
    |> Map.put(:cycle_count, cpu.cycle_count + cycle)
  end

  # load value read at address into the accumulator
  def lda(cpu, addressing_mode) do
    byte_address = AddressingMode.get_address(addressing_mode, cpu)
    byte = Memory.read_byte(cpu.memory, byte_address.address)

    {pc, cycle} =
      case addressing_mode do
        :immediate -> {2, 2}
        :zero_page -> {2, 3}
        :zero_page_x -> {2, 4}
        :absolute -> {3, 4}
        :absolute_x -> if byte_address.page_crossed, do: {3, 4}, else: {3, 5}
        :absolute_y -> if byte_address.page_crossed, do: {3, 4}, else: {3, 5}
        :indexed_indirect -> {2, 6}
        :indirect_indexed -> if byte_address.page_crossed, do: {2, 5}, else: {2, 6}
      end

    cpu
    |> set_register(:a, byte)
    |> set_flag_with_test(:zero, byte)
    |> set_flag_with_test(:negative, byte)
    |> Map.put(:program_counter, cpu.program_counter + pc)
    |> Map.put(:cycle_count, cpu.cycle_count + cycle)
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

  # stores the contents of the accumulator into memory
  def sta(cpu, addressing_mode) do
    address = AddressingMode.get_address(addressing_mode, cpu)
    memory = Memory.write_byte(cpu.memory, address.address, cpu.registers.a)

    {pc, cycle} =
      case addressing_mode do
        :zero_page -> {2, 3}
        :zero_page_x -> {2, 4}
        :absolute -> {3, 4}
        :absolute_x -> {3, 5}
        :absolute_y -> {3, 5}
        :indexed_indirect -> {2, 6}
        :indirect_indexed -> {2, 6}
      end

    %{cpu | memory: memory, program_counter: cpu.program_counter + pc, cycle_count: cpu.cycle_count + cycle}
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

  # return to the calling routine at the end of a subroutine
  def rts(cpu) do
    {cpu, high} = pop_byte_off_stack(cpu)
    {cpu, low} = pop_byte_off_stack(cpu)

    address = high ||| low <<< 8

    %{cpu | program_counter: address + 1, cycle_count: cpu.cycle_count + 6}
  end

  # branch if the given function evaluates to true
  def branch_if(cpu, fun) do
    if fun.(cpu) do
      byte_address = AddressingMode.get_address(:relative, cpu)
      cycles = if byte_address.page_crossed, do: 4, else: 3
      %{cpu | program_counter: byte_address.address, cycle_count: cpu.cycle_count + cycles}
    else
      %{cpu | program_counter: cpu.program_counter + 2, cycle_count: cpu.cycle_count + 2}
    end
  end

  # push a copy of the flag register onto the stack
  def php(cpu) do
    # the unused flags need to be set before pushing the value only
    byte =
      cpu.registers.p
      |> bor(@flags[:unused_1])
      |> bor(@flags[:unused_2])

    cpu
    |> push_byte_onto_stack(byte)
    |> Map.put(:program_counter, cpu.program_counter + 1)
    |> Map.put(:cycle_count, cpu.cycle_count + 3)
  end

  # push a copy of the accumulator onto the stack
  def pha(cpu) do
    cpu
    |> push_byte_onto_stack(cpu.registers.a)
    |> Map.put(:program_counter, cpu.program_counter + 1)
    |> Map.put(:cycle_count, cpu.cycle_count + 3)
  end

  # pop a byte off the stack and store it in the accumlator
  def pla(cpu) do
    {cpu, byte} = pop_byte_off_stack(cpu)

    cpu
    |> set_register(:a, byte)
    |> set_flag_with_test(:zero, byte)
    |> set_flag_with_test(:negative, byte)
    |> Map.put(:program_counter, cpu.program_counter + 1)
    |> Map.put(:cycle_count, cpu.cycle_count + 4)
  end

  # pop a byte off the stack and set flags based on its value
  def plp(cpu) do
    {cpu, byte} = pop_byte_off_stack(cpu)

    # make the unused flags match the nestest logs
    byte = byte &&& bnot(@flags[:unused_1])
    byte = bor(byte, @flags[:unused_2])

    cpu
    |> set_register(:p, byte)
    |> Map.put(:program_counter, cpu.program_counter + 1)
    |> Map.put(:cycle_count, cpu.cycle_count + 4)
  end
end
