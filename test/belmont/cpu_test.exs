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

    test "set_flag_with_test will either set or unset the flag based on the test byte" do
      cpu =
        %CPU{}
        |> CPU.set_flag_with_test(:zero, 0x00)
        |> CPU.set_flag_with_test(:negative, 0b00000000)
        |> CPU.set_flag_with_test(:overflow, 0b00000000)

      assert CPU.flag_set?(cpu, :zero)
      assert !CPU.flag_set?(cpu, :overflow)
      assert !CPU.flag_set?(cpu, :negative)

      cpu =
        %CPU{}
        |> CPU.set_flag_with_test(:zero, 0x01)
        |> CPU.set_flag_with_test(:negative, 0b10000000)
        |> CPU.set_flag_with_test(:overflow, 0b01000000)

      assert !CPU.flag_set?(cpu, :zero)
      assert CPU.flag_set?(cpu, :negative)
      assert CPU.flag_set?(cpu, :overflow)
    end
  end

  describe "The stack" do
    setup do
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

      {:ok, cpu: cpu}
    end

    test "A byte can be pushed", %{cpu: cpu} do
      cpu = CPU.push_byte_onto_stack(cpu, 0x31)
      assert Memory.read_byte(cpu.memory, 0x1FD) == 0x31
      assert cpu.stack_pointer == 0xFC
    end

    test "A word can be pushed", %{cpu: cpu} do
      cpu = CPU.push_word_onto_stack(cpu, 0xC5D1)
      assert Memory.read_byte(cpu.memory, 0x1FD) == 0xC5
      assert Memory.read_byte(cpu.memory, 0x1FC) == 0xD1
      assert cpu.stack_pointer == 0xFB
    end

    test "A byte can be popped off", %{cpu: cpu} do
      assert {cpu, 0x31} =
               CPU.push_byte_onto_stack(cpu, 0x31)
               |> CPU.pop_byte_off_stack()

      assert cpu.stack_pointer == 0xFD
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

  describe "bvs/2" do
    test "adds the relative byte to the program counter if overflow flag is set" do
      cpu =
        FakeROM.rom(
          prg_rom_data_override: [
            [bank: 0, location: 0x0000, value: 0x70],
            [bank: 0, location: 0x0001, value: 0x04]
          ]
        )
        |> Cartridge.parse_rom_contents!()
        |> Memory.new()
        |> CPU.new()
        |> Map.put(:program_counter, 0x8000)
        |> CPU.set_flag(:overflow)
        |> CPU.bvs(:relative)

      assert cpu.program_counter == 0x8006
    end

    test "does not branch if overflow is not set" do
      cpu =
        FakeROM.rom(
          prg_rom_data_override: [
            [bank: 0, location: 0x0000, value: 0x70],
            [bank: 0, location: 0x0001, value: 0x04]
          ]
        )
        |> Cartridge.parse_rom_contents!()
        |> Memory.new()
        |> CPU.new()
        |> Map.put(:program_counter, 0x8000)
        |> CPU.bvs(:relative)

      assert cpu.program_counter == 0x8002
    end
  end

  describe "bvc/2" do
    test "adds the relative byte to the program counter if overflow flag is not set" do
      cpu =
        FakeROM.rom(
          prg_rom_data_override: [
            [bank: 0, location: 0x0000, value: 0x50],
            [bank: 0, location: 0x0001, value: 0x04]
          ]
        )
        |> Cartridge.parse_rom_contents!()
        |> Memory.new()
        |> CPU.new()
        |> Map.put(:program_counter, 0x8000)
        |> CPU.bvc(:relative)

      assert cpu.program_counter == 0x8006
    end

    test "does not branch if overflow is not set" do
      cpu =
        FakeROM.rom(
          prg_rom_data_override: [
            [bank: 0, location: 0x0000, value: 0x50],
            [bank: 0, location: 0x0001, value: 0x04]
          ]
        )
        |> Cartridge.parse_rom_contents!()
        |> Memory.new()
        |> CPU.new()
        |> CPU.set_flag(:overflow)
        |> Map.put(:program_counter, 0x8000)
        |> CPU.bvc(:relative)

      assert cpu.program_counter == 0x8002
    end
  end

  describe "bit/2" do
    setup do
      cpu =
        FakeROM.rom(
          prg_rom_data_override: [
            [bank: 0, location: 0x0000, value: 0x24],
            [bank: 0, location: 0x0001, value: 0xFF],
            [bank: 0, location: 0x0002, value: 0x00]
          ]
        )
        |> Cartridge.parse_rom_contents!()
        |> Memory.new()
        |> CPU.new()
        |> Map.put(:program_counter, 0x8000)

      {:ok, cpu: cpu}
    end

    test "sets negative and overflow flags", %{cpu: cpu} do
      mem = Memory.write_byte(cpu.memory, 0x00FF, 0xFF)

      cpu =
        cpu
        |> CPU.set_register(:a, 0xFF)
        |> Map.put(:memory, mem)
        |> CPU.bit(:zero_page)

      assert CPU.flag_set?(cpu, :negative)
      assert CPU.flag_set?(cpu, :overflow)
      assert !CPU.flag_set?(cpu, :zero)
    end

    test "sets the zero flag", %{cpu: cpu} do
      mem = Memory.write_byte(cpu.memory, 0x00FF, 0xFF)

      cpu =
        cpu
        |> CPU.set_register(:a, 0x00)
        |> Map.put(:memory, mem)
        |> CPU.bit(:zero_page)

      assert CPU.flag_set?(cpu, :zero)
    end
  end

  describe "lda/2" do
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

    test "reads a value from memory and sets it to the accumulator", %{cpu: cpu} do
      cpu = CPU.lda(cpu, :immediate)
      assert cpu.registers.a == 0x75
    end

    test "sets the negative flag", %{cpu: cpu} do
      cpu = Map.put(cpu, :program_counter, 0x8002) |> CPU.lda(:immediate)
      assert CPU.flag_set?(cpu, :negative) == true
    end

    test "sets the zero flag", %{cpu: cpu} do
      cpu = Map.put(cpu, :program_counter, 0x8004) |> CPU.lda(:immediate)
      assert CPU.flag_set?(cpu, :zero) == true
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

  describe "stx/2" do
    test "stores the x register in memory" do
      cpu =
        FakeROM.rom(
          prg_rom_data_override: [
            [bank: 0, location: 0x0000, value: 0x86],
            [bank: 0, location: 0x0001, value: 0x00]
          ]
        )
        |> Cartridge.parse_rom_contents!()
        |> Memory.new()
        |> CPU.new()
        |> Map.put(:program_counter, 0x8000)
        |> CPU.set_register(:x, 0x31)
        |> CPU.stx(:zero_page)

      assert Memory.read_byte(cpu.memory, 0x0000) == 0x31
    end
  end

  describe "jsr/2" do
    test "should push the return point onto the stack and jump to a location" do
      cpu =
        FakeROM.rom(
          prg_rom_data_override: [
            [bank: 0, location: 0x0001, value: 0x2D],
            [bank: 0, location: 0x0002, value: 0xC7]
          ]
        )
        |> Cartridge.parse_rom_contents!()
        |> Memory.new()
        |> CPU.new()
        |> Map.put(:program_counter, 0x8000)
        |> CPU.jsr(:absolute)

      assert cpu.program_counter == 0xC72D
      assert {cpu, 0x02} = CPU.pop_byte_off_stack(cpu)
      assert {_cpu, 0x80} = CPU.pop_byte_off_stack(cpu)
    end
  end
end
