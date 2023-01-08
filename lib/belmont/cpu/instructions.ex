defmodule Belmont.CPU.Instructions do
  @moduledoc """
  Defines all of the 6502 instructions.
  """

  import Belmont.CPU.Definstr
  alias Belmont.CPU

  definstr x01(cpu, "ORA", :byte), do: CPU.logical_op(cpu, :indexed_indirect, :or)
  definstr x03(cpu, "*SLO", :byte), do: CPU.slo(cpu, :indexed_indirect)
  definstr x04(cpu, "*NOP", :byte), do: CPU.nop(cpu, :zero_page)
  definstr x05(cpu, "ORA", :byte), do: CPU.logical_op(cpu, :zero_page, :or)
  definstr x06(cpu, "ASL", :byte), do: CPU.asl(cpu, :zero_page)
  definstr x07(cpu, "*SLO", :byte), do: CPU.slo(cpu, :zero_page)
  definstr x08(cpu, "PHP", :none), do: CPU.php(cpu)
  definstr x09(cpu, "ORA", :byte), do: CPU.logical_op(cpu, :immediate, :or)
  definstr x0A(cpu, "ASL", :none), do: CPU.asl(cpu, :accumulator)
  definstr x0C(cpu, "*NOP", :word), do: CPU.nop(cpu, :absolute)
  definstr x0D(cpu, "ORA", :word), do: CPU.logical_op(cpu, :absolute, :or)
  definstr x0E(cpu, "ASL", :word), do: CPU.asl(cpu, :absolute)
  definstr x0F(cpu, "*SLO", :word), do: CPU.slo(cpu, :absolute)
  definstr x10(cpu, "BPL", :byte), do: CPU.branch_if(cpu, fn cpu -> !CPU.flag_set?(cpu, :negative) end)
  definstr x11(cpu, "ORA", :byte), do: CPU.logical_op(cpu, :indirect_indexed, :or)
  definstr x13(cpu, "*SLO", :byte), do: CPU.slo(cpu, :indirect_indexed)
  definstr x14(cpu, "*NOP", :byte), do: CPU.nop(cpu, :zero_page_x)
  definstr x15(cpu, "ORA", :byte), do: CPU.logical_op(cpu, :zero_page_x, :or)
  definstr x16(cpu, "ASL", :byte), do: CPU.asl(cpu, :zero_page_x)
  definstr x17(cpu, "*SLO", :byte), do: CPU.slo(cpu, :zero_page_x)
  definstr x18(cpu, "CLC", :none), do: CPU.unset_flag_op(cpu, :carry)
  definstr x19(cpu, "ORA", :word), do: CPU.logical_op(cpu, :absolute_y, :or)
  definstr x1A(cpu, "*NOP", :none), do: CPU.nop(cpu, :implied)
  definstr x1B(cpu, "*SLO", :word), do: CPU.slo(cpu, :absolute_y)
  definstr x1C(cpu, "*NOP", :word), do: CPU.nop(cpu, :absolute_x)
  definstr x1D(cpu, "ORA", :word), do: CPU.logical_op(cpu, :absolute_x, :or)
  definstr x1E(cpu, "ASL", :word), do: CPU.asl(cpu, :absolute_x)
  definstr x1F(cpu, "*SLO", :word), do: CPU.slo(cpu, :absolute_x)
  definstr x20(cpu, "JSR", :word), do: CPU.jsr(cpu, :absolute)
  definstr x21(cpu, "AND", :byte), do: CPU.logical_op(cpu, :indexed_indirect, :and)
  definstr x23(cpu, "*RLA", :byte), do: CPU.rla(cpu, :indexed_indirect)
  definstr x24(cpu, "BIT", :byte), do: CPU.bit(cpu, :zero_page)
  definstr x25(cpu, "AND", :byte), do: CPU.logical_op(cpu, :zero_page, :and)
  definstr x26(cpu, "ROL", :byte), do: CPU.rol(cpu, :zero_page)
  definstr x27(cpu, "*RLA", :byte), do: CPU.rla(cpu, :zero_page)
  definstr x28(cpu, "PLP", :none), do: CPU.plp(cpu)
  definstr x29(cpu, "AND", :byte), do: CPU.logical_op(cpu, :immediate, :and)
  definstr x2A(cpu, "ROL", :none), do: CPU.rol(cpu, :accumulator)
  definstr x2C(cpu, "BIT", :word), do: CPU.bit(cpu, :absolute)
  definstr x2D(cpu, "AND", :word), do: CPU.logical_op(cpu, :absolute, :and)
  definstr x2E(cpu, "ROL", :word), do: CPU.rol(cpu, :absolute)
  definstr x2F(cpu, "*RLA", :word), do: CPU.rla(cpu, :absolute)
  definstr x30(cpu, "BMI", :byte), do: CPU.branch_if(cpu, fn cpu -> CPU.flag_set?(cpu, :negative) end)
  definstr x31(cpu, "AND", :byte), do: CPU.logical_op(cpu, :indirect_indexed, :and)
  definstr x33(cpu, "*RLA", :byte), do: CPU.rla(cpu, :indirect_indexed)
  definstr x34(cpu, "*NOP", :byte), do: CPU.nop(cpu, :zero_page_x)
  definstr x35(cpu, "AND", :byte), do: CPU.logical_op(cpu, :zero_page_x, :and)
  definstr x36(cpu, "ROL", :byte), do: CPU.rol(cpu, :zero_page_x)
  definstr x37(cpu, "*RLA", :byte), do: CPU.rla(cpu, :zero_page_x)
  definstr x38(cpu, "SEC", :none), do: CPU.set_flag_op(cpu, :carry)
  definstr x39(cpu, "AND", :word), do: CPU.logical_op(cpu, :absolute_y, :and)
  definstr x3A(cpu, "*NOP", :none), do: CPU.nop(cpu, :implied)
  definstr x3B(cpu, "*RLA", :word), do: CPU.rla(cpu, :absolute_y)
  definstr x3C(cpu, "*NOP", :word), do: CPU.nop(cpu, :absolute_x)
  definstr x3D(cpu, "AND", :word), do: CPU.logical_op(cpu, :absolute_x, :and)
  definstr x3E(cpu, "ROL", :word), do: CPU.rol(cpu, :absolute_x)
  definstr x3F(cpu, "*RLA", :word), do: CPU.rla(cpu, :absolute_x)
  definstr x40(cpu, "RTI", :none), do: CPU.rti(cpu)
  definstr x41(cpu, "EOR", :byte), do: CPU.logical_op(cpu, :indexed_indirect, :eor)
  definstr x43(cpu, "*SRE", :byte), do: CPU.sre(cpu, :indexed_indirect)
  definstr x44(cpu, "*NOP", :byte), do: CPU.nop(cpu, :zero_page)
  definstr x45(cpu, "EOR", :byte), do: CPU.logical_op(cpu, :zero_page, :eor)
  definstr x46(cpu, "LSR", :byte), do: CPU.lsr(cpu, :zero_page)
  definstr x47(cpu, "*SRE", :byte), do: CPU.sre(cpu, :zero_page)
  definstr x48(cpu, "PHA", :none), do: CPU.pha(cpu)
  definstr x49(cpu, "EOR", :byte), do: CPU.logical_op(cpu, :immediate, :eor)
  definstr x4A(cpu, "LSR", :none), do: CPU.lsr(cpu, :accumulator)
  definstr x4C(cpu, "JMP", :word), do: CPU.jmp(cpu, :absolute)
  definstr x4D(cpu, "EOR", :word), do: CPU.logical_op(cpu, :absolute, :eor)
  definstr x4E(cpu, "LSR", :word), do: CPU.lsr(cpu, :absolute)
  definstr x4F(cpu, "*SRE", :word), do: CPU.sre(cpu, :absolute)
  definstr x50(cpu, "BVC", :byte), do: CPU.branch_if(cpu, fn cpu -> !CPU.flag_set?(cpu, :overflow) end)
  definstr x51(cpu, "EOR", :byte), do: CPU.logical_op(cpu, :indirect_indexed, :eor)
  definstr x53(cpu, "*SRE", :byte), do: CPU.sre(cpu, :indirect_indexed)
  definstr x54(cpu, "*NOP", :byte), do: CPU.nop(cpu, :zero_page_x)
  definstr x55(cpu, "EOR", :byte), do: CPU.logical_op(cpu, :zero_page_x, :eor)
  definstr x56(cpu, "LSR", :byte), do: CPU.lsr(cpu, :zero_page_x)
  definstr x57(cpu, "*SRE", :byte), do: CPU.sre(cpu, :zero_page_x)
  definstr x59(cpu, "EOR", :word), do: CPU.logical_op(cpu, :absolute_y, :eor)
  definstr x5A(cpu, "*NOP", :none), do: CPU.nop(cpu, :implied)
  definstr x5B(cpu, "*SRE", :word), do: CPU.sre(cpu, :absolute_y)
  definstr x5C(cpu, "*NOP", :word), do: CPU.nop(cpu, :absolute_x)
  definstr x5D(cpu, "EOR", :word), do: CPU.logical_op(cpu, :absolute_x, :eor)
  definstr x5E(cpu, "LSR", :word), do: CPU.lsr(cpu, :absolute_x)
  definstr x5F(cpu, "*SRE", :word), do: CPU.sre(cpu, :absolute_x)
  definstr x60(cpu, "RTS", :none), do: CPU.rts(cpu)
  definstr x61(cpu, "ADC", :byte), do: CPU.adc(cpu, :indexed_indirect)
  definstr x63(cpu, "*RRA", :byte), do: CPU.rra(cpu, :indexed_indirect)
  definstr x64(cpu, "*NOP", :byte), do: CPU.nop(cpu, :zero_page)
  definstr x65(cpu, "ADC", :byte), do: CPU.adc(cpu, :zero_page)
  definstr x66(cpu, "ROR", :byte), do: CPU.ror(cpu, :zero_page)
  definstr x67(cpu, "*RRA", :byte), do: CPU.rra(cpu, :zero_page)
  definstr x68(cpu, "PLA", :none), do: CPU.pla(cpu)
  definstr x69(cpu, "ADC", :byte), do: CPU.adc(cpu, :immediate)
  definstr x6A(cpu, "ROR", :none), do: CPU.ror(cpu, :accumulator)
  definstr x6C(cpu, "JMP", :word), do: CPU.jmp(cpu, :indirect)
  definstr x6D(cpu, "ADC", :word), do: CPU.adc(cpu, :absolute)
  definstr x6E(cpu, "ROR", :word), do: CPU.ror(cpu, :absolute)
  definstr x6F(cpu, "*RRA", :word), do: CPU.rra(cpu, :absolute)
  definstr x70(cpu, "BVS", :byte), do: CPU.branch_if(cpu, fn cpu -> CPU.flag_set?(cpu, :overflow) end)
  definstr x71(cpu, "ADC", :byte), do: CPU.adc(cpu, :indirect_indexed)
  definstr x73(cpu, "*RRA", :byte), do: CPU.rra(cpu, :indirect_indexed)
  definstr x74(cpu, "*NOP", :byte), do: CPU.nop(cpu, :zero_page_x)
  definstr x75(cpu, "ADC", :byte), do: CPU.adc(cpu, :zero_page_x)
  definstr x76(cpu, "ROR", :byte), do: CPU.ror(cpu, :zero_page_x)
  definstr x77(cpu, "*RRA", :byte), do: CPU.rra(cpu, :zero_page_x)
  definstr x78(cpu, "SEI", :none), do: CPU.set_flag_op(cpu, :interrupt)
  definstr x79(cpu, "ADC", :word), do: CPU.adc(cpu, :absolute_y)
  definstr x7A(cpu, "*NOP", :none), do: CPU.nop(cpu, :implied)
  definstr x7B(cpu, "*RRA", :word), do: CPU.rra(cpu, :absolute_y)
  definstr x7C(cpu, "*NOP", :word), do: CPU.nop(cpu, :absolute_x)
  definstr x7D(cpu, "ADC", :word), do: CPU.adc(cpu, :absolute_x)
  definstr x7E(cpu, "ROR", :word), do: CPU.ror(cpu, :absolute_x)
  definstr x7F(cpu, "*RRA", :word), do: CPU.rra(cpu, :absolute_x)
  definstr x80(cpu, "*NOP", :byte), do: CPU.nop(cpu, :immediate)
  definstr x81(cpu, "STA", :byte), do: CPU.store_register(cpu, :indexed_indirect, :a)
  definstr x83(cpu, "*SAX", :byte), do: CPU.sax(cpu, :indexed_indirect)
  definstr x84(cpu, "STY", :byte), do: CPU.store_register(cpu, :zero_page, :y)
  definstr x85(cpu, "STA", :byte), do: CPU.store_register(cpu, :zero_page, :a)
  definstr x86(cpu, "STX", :byte), do: CPU.store_register(cpu, :zero_page, :x)
  definstr x87(cpu, "*SAX", :byte), do: CPU.sax(cpu, :zero_page)
  definstr x88(cpu, "DEY", :none), do: CPU.decrement_register(cpu, :y)
  definstr x8A(cpu, "TXA", :none), do: CPU.transfer_accumulator(cpu, :x, :a)
  definstr x8C(cpu, "STY", :word), do: CPU.store_register(cpu, :absolute, :y)
  definstr x8D(cpu, "STA", :word), do: CPU.store_register(cpu, :absolute, :a)
  definstr x8E(cpu, "STX", :word), do: CPU.store_register(cpu, :absolute, :x)
  definstr x8F(cpu, "*SAX", :word), do: CPU.sax(cpu, :absolute)
  definstr x90(cpu, "BCC", :byte), do: CPU.branch_if(cpu, fn cpu -> !CPU.flag_set?(cpu, :carry) end)
  definstr x91(cpu, "STA", :byte), do: CPU.store_register(cpu, :indirect_indexed, :a)
  definstr x94(cpu, "STY", :byte), do: CPU.store_register(cpu, :zero_page_x, :y)
  definstr x95(cpu, "STA", :byte), do: CPU.store_register(cpu, :zero_page_x, :a)
  definstr x96(cpu, "STX", :byte), do: CPU.store_register(cpu, :zero_page_y, :x)
  definstr x97(cpu, "*SAX", :byte), do: CPU.sax(cpu, :zero_page_y)
  definstr x98(cpu, "TYA", :none), do: CPU.transfer_accumulator(cpu, :y, :a)
  definstr x99(cpu, "STA", :word), do: CPU.store_register(cpu, :absolute_y, :a)
  definstr x9A(cpu, "TXS", :none), do: CPU.transfer_x_stack(cpu)
  definstr x9D(cpu, "STA", :word), do: CPU.store_register(cpu, :absolute_x, :a)
  definstr xA0(cpu, "LDY", :byte), do: CPU.load_register(cpu, :immediate, :y)
  definstr xA1(cpu, "LDA", :byte), do: CPU.load_register(cpu, :indexed_indirect, :a)
  definstr xA2(cpu, "LDX", :byte), do: CPU.load_register(cpu, :immediate, :x)
  definstr xA3(cpu, "*LAX", :byte), do: CPU.lax(cpu, :indexed_indirect)
  definstr xA4(cpu, "LDY", :byte), do: CPU.load_register(cpu, :zero_page, :y)
  definstr xA5(cpu, "LDA", :byte), do: CPU.load_register(cpu, :zero_page, :a)
  definstr xA6(cpu, "LDX", :byte), do: CPU.load_register(cpu, :zero_page, :x)
  definstr xA7(cpu, "*LAX", :byte), do: CPU.lax(cpu, :zero_page)
  definstr xA8(cpu, "TAY", :none), do: CPU.transfer_accumulator(cpu, :a, :y)
  definstr xA9(cpu, "LDA", :byte), do: CPU.load_register(cpu, :immediate, :a)
  definstr xAA(cpu, "TAX", :none), do: CPU.transfer_accumulator(cpu, :a, :x)
  definstr xAC(cpu, "LDY", :word), do: CPU.load_register(cpu, :absolute, :y)
  definstr xAD(cpu, "LDA", :word), do: CPU.load_register(cpu, :absolute, :a)
  definstr xAE(cpu, "LDX", :word), do: CPU.load_register(cpu, :absolute, :x)
  definstr xAF(cpu, "*LAX", :word), do: CPU.lax(cpu, :absolute)
  definstr xB0(cpu, "BCS", :byte), do: CPU.branch_if(cpu, fn cpu -> CPU.flag_set?(cpu, :carry) end)
  definstr xB1(cpu, "LDA", :byte), do: CPU.load_register(cpu, :indirect_indexed, :a)
  definstr xB3(cpu, "*LAX", :byte), do: CPU.lax(cpu, :indirect_indexed)
  definstr xB4(cpu, "LDY", :byte), do: CPU.load_register(cpu, :zero_page_x, :y)
  definstr xB5(cpu, "LDA", :byte), do: CPU.load_register(cpu, :zero_page_x, :a)
  definstr xB6(cpu, "LDX", :byte), do: CPU.load_register(cpu, :zero_page_y, :x)
  definstr xB7(cpu, "*LAX", :byte), do: CPU.lax(cpu, :zero_page_y)
  definstr xB8(cpu, "CLV", :none), do: CPU.unset_flag_op(cpu, :overflow)
  definstr xBA(cpu, "TSX", :none), do: CPU.transfer_stack_x(cpu)
  definstr xB9(cpu, "LDA", :word), do: CPU.load_register(cpu, :absolute_y, :a)
  definstr xBC(cpu, "LDY", :word), do: CPU.load_register(cpu, :absolute_x, :y)
  definstr xBD(cpu, "LDA", :word), do: CPU.load_register(cpu, :absolute_x, :a)
  definstr xBE(cpu, "LDX", :word), do: CPU.load_register(cpu, :absolute_y, :x)
  definstr xBF(cpu, "*LAX", :word), do: CPU.lax(cpu, :absolute_y)
  definstr xC0(cpu, "CPY", :byte), do: CPU.compare(cpu, :immediate, :y)
  definstr xC1(cpu, "CMP", :byte), do: CPU.compare(cpu, :indexed_indirect, :a)
  definstr xC3(cpu, "*DCP", :byte), do: CPU.dcp(cpu, :indexed_indirect)
  definstr xC4(cpu, "CPY", :byte), do: CPU.compare(cpu, :zero_page, :y)
  definstr xC5(cpu, "CMP", :byte), do: CPU.compare(cpu, :zero_page, :a)
  definstr xC6(cpu, "DEC", :byte), do: CPU.decrement_memory(cpu, :zero_page)
  definstr xC7(cpu, "*DCP", :byte), do: CPU.dcp(cpu, :zero_page)
  definstr xC8(cpu, "INY", :none), do: CPU.increment_register(cpu, :y)
  definstr xC9(cpu, "CMP", :byte), do: CPU.compare(cpu, :immediate, :a)
  definstr xCA(cpu, "DEX", :none), do: CPU.decrement_register(cpu, :x)
  definstr xCC(cpu, "CPY", :word), do: CPU.compare(cpu, :absolute, :y)
  definstr xCD(cpu, "CMP", :word), do: CPU.compare(cpu, :absolute, :a)
  definstr xCE(cpu, "DEC", :word), do: CPU.decrement_memory(cpu, :absolute)
  definstr xCF(cpu, "*DCP", :word), do: CPU.dcp(cpu, :absolute)
  definstr xD0(cpu, "BNE", :byte), do: CPU.branch_if(cpu, fn cpu -> !CPU.flag_set?(cpu, :zero) end)
  definstr xD1(cpu, "CMP", :byte), do: CPU.compare(cpu, :indirect_indexed, :a)
  definstr xD3(cpu, "*DCP", :byte), do: CPU.dcp(cpu, :indirect_indexed)
  definstr xD4(cpu, "*NOP", :byte), do: CPU.nop(cpu, :zero_page_x)
  definstr xD5(cpu, "CMP", :byte), do: CPU.compare(cpu, :zero_page_x, :a)
  definstr xD7(cpu, "*DCP", :byte), do: CPU.dcp(cpu, :zero_page_x)
  definstr xD8(cpu, "CLD", :none), do: CPU.unset_flag_op(cpu, :decimal)
  definstr xD6(cpu, "DEC", :byte), do: CPU.decrement_memory(cpu, :zero_page_x)
  definstr xD9(cpu, "CMP", :word), do: CPU.compare(cpu, :absolute_y, :a)
  definstr xDA(cpu, "*NOP", :none), do: CPU.nop(cpu, :implied)
  definstr xDB(cpu, "*DCP", :word), do: CPU.dcp(cpu, :absolute_y)
  definstr xDC(cpu, "*NOP", :word), do: CPU.nop(cpu, :absolute_x)
  definstr xDD(cpu, "CMP", :word), do: CPU.compare(cpu, :absolute_x, :a)
  definstr xDE(cpu, "DEC", :word), do: CPU.decrement_memory(cpu, :absolute_x)
  definstr xDF(cpu, "*DCP", :word), do: CPU.dcp(cpu, :absolute_x)
  definstr xE0(cpu, "CPX", :byte), do: CPU.compare(cpu, :immediate, :x)
  definstr xE1(cpu, "SBC", :byte), do: CPU.sbc(cpu, :indexed_indirect)
  definstr xE3(cpu, "*ISB", :byte), do: CPU.isc(cpu, :indexed_indirect)
  definstr xE4(cpu, "CPX", :byte), do: CPU.compare(cpu, :zero_page, :x)
  definstr xE5(cpu, "SBC", :byte), do: CPU.sbc(cpu, :zero_page)
  definstr xE6(cpu, "INC", :byte), do: CPU.increment_memory(cpu, :zero_page)
  definstr xE7(cpu, "*ISB", :byte), do: CPU.isc(cpu, :zero_page)
  definstr xE8(cpu, "INX", :none), do: CPU.increment_register(cpu, :x)
  definstr xE9(cpu, "SBC", :byte), do: CPU.sbc(cpu, :immediate)
  definstr xEA(cpu, "NOP", :none), do: CPU.nop(cpu, :implied)
  definstr xEB(cpu, "*SBC", :byte), do: CPU.sbc(cpu, :immediate)
  definstr xEC(cpu, "CPX", :word), do: CPU.compare(cpu, :absolute, :x)
  definstr xED(cpu, "SBC", :word), do: CPU.sbc(cpu, :absolute)
  definstr xEE(cpu, "INC", :word), do: CPU.increment_memory(cpu, :absolute)
  definstr xEF(cpu, "*ISB", :word), do: CPU.isc(cpu, :absolute)
  definstr xF0(cpu, "BEQ", :byte), do: CPU.branch_if(cpu, fn cpu -> CPU.flag_set?(cpu, :zero) end)
  definstr xF1(cpu, "SBC", :byte), do: CPU.sbc(cpu, :indirect_indexed)
  definstr xF3(cpu, "*ISB", :byte), do: CPU.isc(cpu, :indirect_indexed)
  definstr xF4(cpu, "*NOP", :byte), do: CPU.nop(cpu, :zero_page_x)
  definstr xF5(cpu, "SBC", :byte), do: CPU.sbc(cpu, :zero_page_x)
  definstr xF6(cpu, "INC", :byte), do: CPU.increment_memory(cpu, :zero_page_x)
  definstr xF7(cpu, "*ISB", :byte), do: CPU.isc(cpu, :zero_page_x)
  definstr xF8(cpu, "SED", :none), do: CPU.set_flag_op(cpu, :decimal)
  definstr xF9(cpu, "SBC", :word), do: CPU.sbc(cpu, :absolute_y)
  definstr xFA(cpu, "*NOP", :none), do: CPU.nop(cpu, :implied)
  definstr xFB(cpu, "*ISB", :word), do: CPU.isc(cpu, :absolute_y)
  definstr xFC(cpu, "*NOP", :word), do: CPU.nop(cpu, :absolute_x)
  definstr xFD(cpu, "SBC", :word), do: CPU.sbc(cpu, :absolute_x)
  definstr xFE(cpu, "INC", :word), do: CPU.increment_memory(cpu, :absolute_x)
  definstr xFF(cpu, "*ISB", :word), do: CPU.isc(cpu, :absolute_x)

  # base cases
  def log(cpu, opcode), do: CPU.log_state(cpu, opcode, "UNDEF", :none)

  def execute(cpu, opcode) do
    raise("Undefined opcode: #{Belmont.Hexstr.hex(opcode, 2)} at #{Belmont.Hexstr.hex(cpu.program_counter, 4)}")
  end
end
