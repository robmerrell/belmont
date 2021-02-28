defmodule Belmont.CPUTest do
  use ExUnit.Case
  alias Belmont.CPU

  describe "registers" do
    setup do: {:ok, flags: [:carry, :zero, :interrupt, :overflow, :negative]}

    test "The flag register should be able to have flags set on it", %{flags: flags} do
      # all flags should be unset by default
      for flag <- flags do
        assert !CPU.flag_set?(%CPU{}, flag)
      end

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
end
