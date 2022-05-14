defmodule Belmont.CPU.Instructions do
  @moduledoc """
  Defines all of the 6502 instructions.
  """

  import Belmont.CPU.Definstr
  alias Belmont.CPU

  definstr x08(cpu, "PHP", :none), do: CPU.php(cpu)
  definstr x09(cpu, "ORA", :byte), do: CPU.logical_op(cpu, :immediate, :or)
  definstr x0A(cpu, "ASL", :none), do: CPU.asl(cpu, :accumulator)
  definstr x10(cpu, "BPL", :byte), do: CPU.branch_if(cpu, fn cpu -> !CPU.flag_set?(cpu, :negative) end)
  definstr x18(cpu, "CLC", :none), do: CPU.unset_flag_op(cpu, :carry)
  definstr x20(cpu, "JSR", :word), do: CPU.jsr(cpu, :absolute)
  definstr x24(cpu, "BIT", :byte), do: CPU.bit(cpu, :zero_page)
  definstr x28(cpu, "PLP", :none), do: CPU.plp(cpu)
  definstr x29(cpu, "AND", :byte), do: CPU.logical_op(cpu, :immediate, :and)
  definstr x2C(cpu, "BIT", :word), do: CPU.bit(cpu, :absolute)
  definstr x30(cpu, "BMI", :byte), do: CPU.branch_if(cpu, fn cpu -> CPU.flag_set?(cpu, :negative) end)
  definstr x38(cpu, "SEC", :none), do: CPU.set_flag_op(cpu, :carry)
  definstr x40(cpu, "RTI", :none), do: CPU.rti(cpu)
  definstr x48(cpu, "PHA", :none), do: CPU.pha(cpu)
  definstr x49(cpu, "EOR", :byte), do: CPU.logical_op(cpu, :immediate, :eor)
  definstr x4A(cpu, "LSR", :none), do: CPU.lsr(cpu, :accumulator)
  definstr x4C(cpu, "JMP", :word), do: CPU.jmp(cpu, :absolute)
  definstr x50(cpu, "BVC", :byte), do: CPU.branch_if(cpu, fn cpu -> !CPU.flag_set?(cpu, :overflow) end)
  definstr x60(cpu, "RTS", :none), do: CPU.rts(cpu)
  definstr x68(cpu, "PLA", :none), do: CPU.pla(cpu)
  definstr x69(cpu, "ADC", :byte), do: CPU.adc(cpu, :immediate)
  definstr x6A(cpu, "ROR", :none), do: CPU.ror(cpu, :accumulator)
  definstr x70(cpu, "BVS", :byte), do: CPU.branch_if(cpu, fn cpu -> CPU.flag_set?(cpu, :overflow) end)
  definstr x78(cpu, "SEI", :none), do: CPU.set_flag_op(cpu, :interrupt)
  definstr x85(cpu, "STA", :byte), do: CPU.store_register(cpu, :zero_page, :a)
  definstr x86(cpu, "STX", :byte), do: CPU.store_register(cpu, :zero_page, :x)
  definstr x88(cpu, "DEY", :none), do: CPU.decrement_register(cpu, :y)
  definstr x8A(cpu, "TXA", :none), do: CPU.transfer_accumulator(cpu, :x, :a)
  definstr x8E(cpu, "STX", :word), do: CPU.store_register(cpu, :absolute, :x)
  definstr x90(cpu, "BCC", :byte), do: CPU.branch_if(cpu, fn cpu -> !CPU.flag_set?(cpu, :carry) end)
  definstr x98(cpu, "TYA", :none), do: CPU.transfer_accumulator(cpu, :y, :a)
  definstr x9A(cpu, "TXS", :none), do: CPU.transfer_x_stack(cpu)
  definstr xA0(cpu, "LDY", :byte), do: CPU.load_register(cpu, :immediate, :y)
  definstr xA2(cpu, "LDX", :byte), do: CPU.load_register(cpu, :immediate, :x)
  definstr xA8(cpu, "TAY", :none), do: CPU.transfer_accumulator(cpu, :a, :y)
  definstr xA9(cpu, "LDA", :byte), do: CPU.load_register(cpu, :immediate, :a)
  definstr xAA(cpu, "TAX", :none), do: CPU.transfer_accumulator(cpu, :a, :x)
  definstr xAD(cpu, "LDA", :word), do: CPU.load_register(cpu, :absolute, :a)
  definstr xAE(cpu, "LDX", :word), do: CPU.load_register(cpu, :absolute, :x)
  definstr xB0(cpu, "BCS", :byte), do: CPU.branch_if(cpu, fn cpu -> CPU.flag_set?(cpu, :carry) end)
  definstr xB8(cpu, "CLV", :none), do: CPU.unset_flag_op(cpu, :overflow)
  definstr xBA(cpu, "TSX", :none), do: CPU.transfer_stack_x(cpu)
  definstr xC0(cpu, "CPY", :byte), do: CPU.compare(cpu, :immediate, :y)
  definstr xC8(cpu, "INY", :none), do: CPU.increment_register(cpu, :y)
  definstr xC9(cpu, "CMP", :byte), do: CPU.compare(cpu, :immediate, :a)
  definstr xCA(cpu, "DEX", :none), do: CPU.decrement_register(cpu, :x)
  definstr xD0(cpu, "BNE", :byte), do: CPU.branch_if(cpu, fn cpu -> !CPU.flag_set?(cpu, :zero) end)
  definstr xD8(cpu, "CLD", :none), do: CPU.unset_flag_op(cpu, :decimal)
  definstr xE0(cpu, "CPX", :byte), do: CPU.compare(cpu, :immediate, :x)
  definstr xE8(cpu, "INX", :none), do: CPU.increment_register(cpu, :x)
  definstr xE9(cpu, "SBC", :byte), do: CPU.sbc(cpu, :immediate)
  definstr xEA(cpu, "NOP", :none), do: CPU.nop(cpu, :implied)
  definstr xF0(cpu, "BEQ", :byte), do: CPU.branch_if(cpu, fn cpu -> CPU.flag_set?(cpu, :zero) end)
  definstr xF8(cpu, "SED", :none), do: CPU.set_flag_op(cpu, :decimal)

  # base cases
  def log(cpu, opcode), do: CPU.log_state(cpu, opcode, "UNDEF", :none)

  def execute(cpu, opcode) do
    raise("Undefined opcode: #{Belmont.Hexstr.hex(opcode, 2)} at #{Belmont.Hexstr.hex(cpu.program_counter, 4)}")
  end
end
