defmodule Belmont.CPUTest do
  use ExUnit.Case
  alias Belmont.CPU
  alias Belmont.FakeROM
  alias Belmont.Memory
  alias Belmont.Cartridge

  describe "registers" do
    setup do: {:ok, flags: [:carry, :zero, :interrupt, :overflow, :negative]}

    test "The flag register should be able to have flags set on it" do
      cpu =
        %CPU{}
        |> CPU.set_flag(:zero)
        |> CPU.set_flag(:negative)
        |> CPU.set_flag(:interrupt)

      assert CPU.flag_set?(cpu, :zero)
      assert CPU.flag_set?(cpu, :negative)
      assert CPU.flag_set?(cpu, :interrupt)
      assert !CPU.flag_set?(cpu, :carry)
      assert !CPU.flag_set?(cpu, :overflow)
    end

    test "the flag register should be able to have flags unset", %{flags: flags} do
      cpu = %CPU{registers: %{p: 0xFF}}

      # all flags should be set
      for flag <- flags do
        assert CPU.flag_set?(cpu, flag)
      end

      cpu =
        cpu
        |> CPU.unset_flag(:zero)
        |> CPU.unset_flag(:carry)

      assert !CPU.flag_set?(cpu, :zero)
      assert CPU.flag_set?(cpu, :negative)
      assert CPU.flag_set?(cpu, :interrupt)
      assert !CPU.flag_set?(cpu, :carry)
      assert CPU.flag_set?(cpu, :overflow)
    end

    test "The register bits should be in the right places" do
      assert CPU.flag_set?(%CPU{registers: %{p: 0x01}}, :carry)
      assert CPU.flag_set?(%CPU{registers: %{p: 0x02}}, :zero)
      assert CPU.flag_set?(%CPU{registers: %{p: 0x04}}, :interrupt)
      assert CPU.flag_set?(%CPU{registers: %{p: 0x40}}, :overflow)
      assert CPU.flag_set?(%CPU{registers: %{p: 0x80}}, :negative)
    end

    test "sets a general purpose register" do
      cpu =
        %CPU{}
        |> CPU.set_register(:a, 0x10)
        |> CPU.set_register(:x, 0xF1)
        |> CPU.set_register(:y, 0xA2)

      assert cpu.registers.a == 0x10
      assert cpu.registers.x == 0xF1
      assert cpu.registers.y == 0xA2
    end
  end

  describe "jmp/2" do
    test "jumps to the location read in memory" do
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
        |> CPU.jmp(:absolute)

      assert cpu.program_counter == 0xC5F5
      assert cpu.cycle_count == 3
    end
  end

  describe "ldx/2" do
    setup do
      cpu =
        FakeROM.rom(
          prg_rom_data_override: [
            [bank: 0, location: 0x0000, value: 0xA2],
            [bank: 0, location: 0x0001, value: 0x75],
            [bank: 0, location: 0x0002, value: 0xA2],
            [bank: 0, location: 0x0003, value: 0xFF],
            [bank: 0, location: 0x0004, value: 0xA2],
            [bank: 0, location: 0x0005, value: 0x00]
          ]
        )
        |> Cartridge.parse_rom_contents!()
        |> Memory.new()
        |> CPU.new()
        |> Map.put(:program_counter, 0x8000)

      {:ok, cpu: cpu}
    end

    test "reads a value from memory and sets it to the x register", %{cpu: cpu} do
      cpu = CPU.ldx(cpu, :immediate)
      assert cpu.registers.x == 0x75
    end

    test "sets the negative flag", %{cpu: cpu} do
      cpu = Map.put(cpu, :program_counter, 0x8002) |> CPU.ldx(:immediate)
      assert CPU.flag_set?(cpu, :negative) == true
    end

    test "sets the zero flag", %{cpu: cpu} do
      cpu = Map.put(cpu, :program_counter, 0x8004) |> CPU.ldx(:immediate)
      assert CPU.flag_set?(cpu, :zero) == true
    end
  end
end
