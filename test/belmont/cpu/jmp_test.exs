defmodule Belmont.CPU.JMPTest do
  use ExUnit.Case
  alias Belmont.CPU.JMP
  alias Belmont.FakeROM
  alias Belmont.Memory
  alias Belmont.CPU
  alias Belmont.Cartridge

  describe "absolute/1 should read an address and update the program counter" do
    cpu =
      FakeROM.rom(
        prg_rom_data_override: [
          [bank: 0, location: 0x0000, value: 0x4C],
          [bank: 0, location: 0x0001, value: 0xF5],
          [bank: 0, location: 0x0002, value: 0xC5]
        ]
      )
      |> Cartridge.parse_rom_contents!()
      |> Memory.new()
      |> CPU.new()
      |> Map.put(:program_counter, 0x8000)

    cpu = JMP.absolute(cpu)

    assert cpu.program_counter == 0xC5F5
    assert cpu.cycle_count == 3
  end
end
