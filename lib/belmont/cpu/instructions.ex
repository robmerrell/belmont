defmodule Belmont.CPU.Instructions do
  @moduledoc """
  Defines all of the 6502 instructions.
  """

  import Belmont.CPU.Definstr
  alias Belmont.CPU

  definstr x01(cpu, "ORA", :byte), do: CPU.logical_op(cpu, :indexed_indirect, :or)
  definstr x05(cpu, "ORA", :byte), do: CPU.logical_op(cpu, :zero_page, :or)
  definstr x06(cpu, "ASL", :byte), do: CPU.asl(cpu, :zero_page)
  definstr x08(cpu, "PHP", :none), do: CPU.php(cpu)
  definstr x09(cpu, "ORA", :byte), do: CPU.logical_op(cpu, :immediate, :or)
  definstr x0A(cpu, "ASL", :none), do: CPU.asl(cpu, :accumulator)
  definstr x0D(cpu, "ORA", :word), do: CPU.logical_op(cpu, :absolute, :or)
  definstr x0E(cpu, "ASL", :word), do: CPU.asl(cpu, :absolute)
  definstr x10(cpu, "BPL", :byte), do: CPU.branch_if(cpu, fn cpu -> !CPU.flag_set?(cpu, :negative) end)
  definstr x11(cpu, "ORA", :byte), do: CPU.logical_op(cpu, :indirect_indexed, :or)
  definstr x18(cpu, "CLC", :none), do: CPU.unset_flag_op(cpu, :carry)
  definstr x20(cpu, "JSR", :word), do: CPU.jsr(cpu, :absolute)
  definstr x21(cpu, "AND", :byte), do: CPU.logical_op(cpu, :indexed_indirect, :and)
  definstr x24(cpu, "BIT", :byte), do: CPU.bit(cpu, :zero_page)
  definstr x25(cpu, "AND", :byte), do: CPU.logical_op(cpu, :zero_page, :and)
  definstr x26(cpu, "ROL", :byte), do: CPU.rol(cpu, :zero_page)
  definstr x28(cpu, "PLP", :none), do: CPU.plp(cpu)
  definstr x29(cpu, "AND", :byte), do: CPU.logical_op(cpu, :immediate, :and)
  definstr x2A(cpu, "ROL", :none), do: CPU.rol(cpu, :accumulator)
  definstr x2C(cpu, "BIT", :word), do: CPU.bit(cpu, :absolute)
  definstr x2D(cpu, "AND", :word), do: CPU.logical_op(cpu, :absolute, :and)
  definstr x2E(cpu, "ROL", :word), do: CPU.rol(cpu, :absolute)
  definstr x30(cpu, "BMI", :byte), do: CPU.branch_if(cpu, fn cpu -> CPU.flag_set?(cpu, :negative) end)
  definstr x31(cpu, "AND", :byte), do: CPU.logical_op(cpu, :indirect_indexed, :and)
  definstr x38(cpu, "SEC", :none), do: CPU.set_flag_op(cpu, :carry)
  definstr x40(cpu, "RTI", :none), do: CPU.rti(cpu)
  definstr x41(cpu, "EOR", :byte), do: CPU.logical_op(cpu, :indexed_indirect, :eor)
  definstr x45(cpu, "EOR", :byte), do: CPU.logical_op(cpu, :zero_page, :eor)
  definstr x46(cpu, "LSR", :byte), do: CPU.lsr(cpu, :zero_page)
  definstr x48(cpu, "PHA", :none), do: CPU.pha(cpu)
  definstr x49(cpu, "EOR", :byte), do: CPU.logical_op(cpu, :immediate, :eor)
  definstr x4A(cpu, "LSR", :none), do: CPU.lsr(cpu, :accumulator)
  definstr x4C(cpu, "JMP", :word), do: CPU.jmp(cpu, :absolute)
  definstr x4D(cpu, "EOR", :word), do: CPU.logical_op(cpu, :absolute, :eor)
  definstr x4E(cpu, "LSR", :word), do: CPU.lsr(cpu, :absolute)
  definstr x50(cpu, "BVC", :byte), do: CPU.branch_if(cpu, fn cpu -> !CPU.flag_set?(cpu, :overflow) end)
  definstr x51(cpu, "EOR", :byte), do: CPU.logical_op(cpu, :indirect_indexed, :eor)
  definstr x60(cpu, "RTS", :none), do: CPU.rts(cpu)
  definstr x61(cpu, "ADC", :byte), do: CPU.adc(cpu, :indexed_indirect)
  definstr x65(cpu, "ADC", :byte), do: CPU.adc(cpu, :zero_page)
  definstr x66(cpu, "ROR", :byte), do: CPU.ror(cpu, :zero_page)
  definstr x68(cpu, "PLA", :none), do: CPU.pla(cpu)
  definstr x69(cpu, "ADC", :byte), do: CPU.adc(cpu, :immediate)
  definstr x6A(cpu, "ROR", :none), do: CPU.ror(cpu, :accumulator)
  definstr x6C(cpu, "JMP", :word), do: CPU.jmp(cpu, :indirect)
  definstr x6D(cpu, "ADC", :word), do: CPU.adc(cpu, :absolute)
  definstr x6E(cpu, "ROR", :word), do: CPU.ror(cpu, :absolute)
  definstr x70(cpu, "BVS", :byte), do: CPU.branch_if(cpu, fn cpu -> CPU.flag_set?(cpu, :overflow) end)
  definstr x71(cpu, "ADC", :byte), do: CPU.adc(cpu, :indirect_indexed)
  definstr x78(cpu, "SEI", :none), do: CPU.set_flag_op(cpu, :interrupt)
  definstr x81(cpu, "STA", :byte), do: CPU.store_register(cpu, :indexed_indirect, :a)
  definstr x84(cpu, "STY", :byte), do: CPU.store_register(cpu, :zero_page, :y)
  definstr x85(cpu, "STA", :byte), do: CPU.store_register(cpu, :zero_page, :a)
  definstr x86(cpu, "STX", :byte), do: CPU.store_register(cpu, :zero_page, :x)
  definstr x88(cpu, "DEY", :none), do: CPU.decrement_register(cpu, :y)
  definstr x8A(cpu, "TXA", :none), do: CPU.transfer_accumulator(cpu, :x, :a)
  definstr x8C(cpu, "STY", :word), do: CPU.store_register(cpu, :absolute, :y)
  definstr x8D(cpu, "STA", :word), do: CPU.store_register(cpu, :absolute, :a)
  definstr x8E(cpu, "STX", :word), do: CPU.store_register(cpu, :absolute, :x)
  definstr x90(cpu, "BCC", :byte), do: CPU.branch_if(cpu, fn cpu -> !CPU.flag_set?(cpu, :carry) end)
  definstr x91(cpu, "STA", :byte), do: CPU.store_register(cpu, :indirect_indexed, :a)
  definstr x98(cpu, "TYA", :none), do: CPU.transfer_accumulator(cpu, :y, :a)
  definstr x9A(cpu, "TXS", :none), do: CPU.transfer_x_stack(cpu)
  definstr xA0(cpu, "LDY", :byte), do: CPU.load_register(cpu, :immediate, :y)
  definstr xA1(cpu, "LDA", :byte), do: CPU.load_register(cpu, :indexed_indirect, :a)
  definstr xA2(cpu, "LDX", :byte), do: CPU.load_register(cpu, :immediate, :x)
  definstr xA4(cpu, "LDY", :byte), do: CPU.load_register(cpu, :zero_page, :y)
  definstr xA5(cpu, "LDA", :byte), do: CPU.load_register(cpu, :zero_page, :a)
  definstr xA6(cpu, "LDX", :byte), do: CPU.load_register(cpu, :zero_page, :x)
  definstr xA8(cpu, "TAY", :none), do: CPU.transfer_accumulator(cpu, :a, :y)
  definstr xA9(cpu, "LDA", :byte), do: CPU.load_register(cpu, :immediate, :a)
  definstr xAA(cpu, "TAX", :none), do: CPU.transfer_accumulator(cpu, :a, :x)
  definstr xAC(cpu, "LDY", :word), do: CPU.load_register(cpu, :absolute, :y)
  definstr xAD(cpu, "LDA", :word), do: CPU.load_register(cpu, :absolute, :a)
  definstr xAE(cpu, "LDX", :word), do: CPU.load_register(cpu, :absolute, :x)
  definstr xB0(cpu, "BCS", :byte), do: CPU.branch_if(cpu, fn cpu -> CPU.flag_set?(cpu, :carry) end)
  definstr xB1(cpu, "LDA", :byte), do: CPU.load_register(cpu, :indirect_indexed, :a)
  definstr xB8(cpu, "CLV", :none), do: CPU.unset_flag_op(cpu, :overflow)
  definstr xBA(cpu, "TSX", :none), do: CPU.transfer_stack_x(cpu)
  definstr xB9(cpu, "LDA", :word), do: CPU.load_register(cpu, :absolute_y, :a)
  definstr xC0(cpu, "CPY", :byte), do: CPU.compare(cpu, :immediate, :y)
  definstr xC1(cpu, "CMP", :byte), do: CPU.compare(cpu, :indexed_indirect, :a)
  definstr xC4(cpu, "CPY", :byte), do: CPU.compare(cpu, :zero_page, :y)
  definstr xC5(cpu, "CMP", :byte), do: CPU.compare(cpu, :zero_page, :a)
  definstr xC6(cpu, "DEC", :byte), do: CPU.decrement_memory(cpu, :zero_page)
  definstr xC8(cpu, "INY", :none), do: CPU.increment_register(cpu, :y)
  definstr xC9(cpu, "CMP", :byte), do: CPU.compare(cpu, :immediate, :a)
  definstr xCA(cpu, "DEX", :none), do: CPU.decrement_register(cpu, :x)
  definstr xCC(cpu, "CPY", :word), do: CPU.compare(cpu, :absolute, :y)
  definstr xCD(cpu, "CMP", :word), do: CPU.compare(cpu, :absolute, :a)
  definstr xCE(cpu, "DEC", :word), do: CPU.decrement_memory(cpu, :absolute)
  definstr xD0(cpu, "BNE", :byte), do: CPU.branch_if(cpu, fn cpu -> !CPU.flag_set?(cpu, :zero) end)
  definstr xD1(cpu, "CMP", :byte), do: CPU.compare(cpu, :indirect_indexed, :a)
  definstr xD8(cpu, "CLD", :none), do: CPU.unset_flag_op(cpu, :decimal)
  definstr xE0(cpu, "CPX", :byte), do: CPU.compare(cpu, :immediate, :x)
  definstr xE1(cpu, "SBC", :byte), do: CPU.sbc(cpu, :indexed_indirect)
  definstr xE4(cpu, "CPX", :byte), do: CPU.compare(cpu, :zero_page, :x)
  definstr xE5(cpu, "SBC", :byte), do: CPU.sbc(cpu, :zero_page)
  definstr xE6(cpu, "INC", :byte), do: CPU.increment_memory(cpu, :zero_page)
  definstr xE8(cpu, "INX", :none), do: CPU.increment_register(cpu, :x)
  definstr xE9(cpu, "SBC", :byte), do: CPU.sbc(cpu, :immediate)
  definstr xEA(cpu, "NOP", :none), do: CPU.nop(cpu, :implied)
  definstr xEC(cpu, "CPX", :word), do: CPU.compare(cpu, :absolute, :x)
  definstr xED(cpu, "SBC", :word), do: CPU.sbc(cpu, :absolute)
  definstr xEE(cpu, "INC", :word), do: CPU.increment_memory(cpu, :absolute)
  definstr xF0(cpu, "BEQ", :byte), do: CPU.branch_if(cpu, fn cpu -> CPU.flag_set?(cpu, :zero) end)
  definstr xF1(cpu, "SBC", :byte), do: CPU.sbc(cpu, :indirect_indexed)
  definstr xF8(cpu, "SED", :none), do: CPU.set_flag_op(cpu, :decimal)

  # base cases
  def log(cpu, opcode), do: CPU.log_state(cpu, opcode, "UNDEF", :none)

  def execute(cpu, opcode) do
    raise("Undefined opcode: #{Belmont.Hexstr.hex(opcode, 2)} at #{Belmont.Hexstr.hex(cpu.program_counter, 4)}")
  end
end
