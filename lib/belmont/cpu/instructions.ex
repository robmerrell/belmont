defmodule Belmont.CPU.Instructions do
  @moduledoc """
  Defines all of the 6502 instructions.
  """

  import Belmont.CPU.Definstr
  alias Belmont.CPU

  definstr php(cpu, 0x08, :none), do: CPU.php(cpu)
  definstr ora(cpu, 0x09, :byte), do: CPU.logical_op(cpu, :immediate, :or)
  definstr bpl(cpu, 0x10, :byte), do: CPU.branch_if(cpu, fn cpu -> !CPU.flag_set?(cpu, :negative) end)
  definstr clc(cpu, 0x18, :none), do: CPU.unset_flag_op(cpu, :carry)
  definstr jsr(cpu, 0x20, :word), do: CPU.jsr(cpu, :absolute)
  definstr bit(cpu, 0x24, :byte), do: CPU.bit(cpu, :zero_page)
  definstr plp(cpu, 0x28, :none), do: CPU.plp(cpu)
  definstr and_op(cpu, 0x29, :byte), do: CPU.logical_op(cpu, :immediate, :and)
  definstr bit(cpu, 0x2C, :word), do: CPU.bit(cpu, :absolute)
  definstr bmi(cpu, 0x30, :byte), do: CPU.branch_if(cpu, fn cpu -> CPU.flag_set?(cpu, :negative) end)
  definstr sec(cpu, 0x38, :none), do: CPU.set_flag_op(cpu, :carry)
  definstr pha(cpu, 0x48, :none), do: CPU.pha(cpu)
  definstr eor(cpu, 0x49, :byte), do: CPU.logical_op(cpu, :immediate, :eor)
  definstr jmp(cpu, 0x4C, :word), do: CPU.jmp(cpu, :absolute)
  definstr bvc(cpu, 0x50, :byte), do: CPU.branch_if(cpu, fn cpu -> !CPU.flag_set?(cpu, :overflow) end)
  definstr rts(cpu, 0x60, :none), do: CPU.rts(cpu)
  definstr pla(cpu, 0x68, :none), do: CPU.pla(cpu)
  definstr adc(cpu, 0x69, :byte), do: CPU.adc(cpu, :immediate)
  definstr bvs(cpu, 0x70, :byte), do: CPU.branch_if(cpu, fn cpu -> CPU.flag_set?(cpu, :overflow) end)
  definstr sei(cpu, 0x78, :none), do: CPU.set_flag_op(cpu, :interrupt)
  definstr sta(cpu, 0x85, :byte), do: CPU.store_register(cpu, :zero_page, :a)
  definstr stx(cpu, 0x86, :byte), do: CPU.store_register(cpu, :zero_page, :x)
  definstr dey(cpu, 0x88, :none), do: CPU.decrement_register(cpu, :y)
  definstr txa(cpu, 0x8A, :none), do: CPU.transfer_accumulator(cpu, :x, :a)
  definstr stx(cpu, 0x8E, :word), do: CPU.store_register(cpu, :absolute, :x)
  definstr bcc(cpu, 0x90, :byte), do: CPU.branch_if(cpu, fn cpu -> !CPU.flag_set?(cpu, :carry) end)
  definstr tya(cpu, 0x98, :none), do: CPU.transfer_accumulator(cpu, :y, :a)
  definstr ldy(cpu, 0xA0, :byte), do: CPU.load_register(cpu, :immediate, :y)
  definstr ldx(cpu, 0xA2, :byte), do: CPU.load_register(cpu, :immediate, :x)
  definstr tay(cpu, 0xA8, :none), do: CPU.transfer_accumulator(cpu, :a, :y)
  definstr lda(cpu, 0xA9, :byte), do: CPU.load_register(cpu, :immediate, :a)
  definstr tax(cpu, 0xAA, :none), do: CPU.transfer_accumulator(cpu, :a, :x)
  definstr bcs(cpu, 0xB0, :byte), do: CPU.branch_if(cpu, fn cpu -> CPU.flag_set?(cpu, :carry) end)
  definstr clv(cpu, 0xB8, :none), do: CPU.unset_flag_op(cpu, :overflow)
  definstr tsx(cpu, 0xBA, :none), do: CPU.transfer_stack_x(cpu)
  definstr cpy(cpu, 0xC0, :byte), do: CPU.compare(cpu, :immediate, :y)
  definstr iny(cpu, 0xC8, :none), do: CPU.increment_register(cpu, :y)
  definstr cmp(cpu, 0xC9, :byte), do: CPU.compare(cpu, :immediate, :a)
  definstr dex(cpu, 0xCA, :none), do: CPU.decrement_register(cpu, :x)
  definstr bne(cpu, 0xD0, :byte), do: CPU.branch_if(cpu, fn cpu -> !CPU.flag_set?(cpu, :zero) end)
  definstr cld(cpu, 0xD8, :none), do: CPU.unset_flag_op(cpu, :decimal)
  definstr cpx(cpu, 0xE0, :byte), do: CPU.compare(cpu, :immediate, :x)
  definstr inx(cpu, 0xE8, :none), do: CPU.increment_register(cpu, :x)
  definstr sbc(cpu, 0xE9, :byte), do: CPU.sbc(cpu, :immediate)
  definstr nop(cpu, 0xEA, :none), do: CPU.nop(cpu, :implied)
  definstr beq(cpu, 0xF0, :byte), do: CPU.branch_if(cpu, fn cpu -> CPU.flag_set?(cpu, :zero) end)
  definstr sed(cpu, 0xF8, :none), do: CPU.set_flag_op(cpu, :decimal)

  # base cases
  def log(cpu, opcode), do: CPU.log_state(cpu, opcode, "UNDEF", :none)

  def execute(cpu, opcode) do
    raise("Undefined opcode: #{Belmont.Hexstr.hex(opcode, 2)} at #{Belmont.Hexstr.hex(cpu.program_counter, 4)}")
  end
end
