defmodule Belmont.FakeROM do
  @defaults %{
    fill_prg_rom: 0,
    fill_chr_rom: 0,
    prg_rom_banks_count: 1,
    chr_rom_banks_count: 1,
    prg_ram_banks_count: 1,
    mapper: 2,
    mirroring_mode: :horizontal,
    battery_backed_ram: 0,
    trainer_present: 0
  }

  @doc """
  Generates a fake rom for unit tests
  """
  def rom(options \\ []) do
    %{
      fill_prg_rom: fill_prg_rom,
      fill_chr_rom: fill_chr_rom,
      prg_rom_banks_count: prg_rom_banks_count,
      chr_rom_banks_count: chr_rom_banks_count,
      prg_ram_banks_count: prg_ram_banks_count,
      mapper: mapper,
      mirroring_mode: mirroring_mode,
      battery_backed_ram: battery_backed_ram,
      trainer_present: trainer_present
    } = Enum.into(options, @defaults)

    # Mapper, mirroring, battery, trainer
    <<upper_mapper::4, lower_mapper::4>> = <<mapper>>

    {mirroring_bit, four_screen_mirroring} =
      case mirroring_mode do
        :horizontal -> {0, 0}
        :vertical -> {1, 0}
        _ -> {0, 1}
      end

    flag6 = <<lower_mapper::4, four_screen_mirroring::1, trainer_present::1, battery_backed_ram::1, mirroring_bit::1>>

    # Mapper, VS/Playchoice, NES 2.0
    flag7 = <<upper_mapper::4, 0::1, 0::1, 0::1, 0::1>>

    # header
    header =
      <<0x4E, 0x45, 0x53, 0x1A, prg_rom_banks_count, chr_rom_banks_count>> <>
        flag6 <> flag7 <> <<prg_ram_banks_count, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0>>

    # game data, just fill it with 0
    trainer_bytes =
      if trainer_present == 1 do
        for _ <- 1..512, into: <<>>, do: <<0>>
      else
        <<>>
      end

    prg_data_size = prg_rom_banks_count * 16_384
    prg_data = for _ <- 1..prg_data_size, into: <<>>, do: <<fill_prg_rom>>

    chr_data_size = chr_rom_banks_count * 8_192
    chr_data = for _ <- 1..chr_data_size, into: <<>>, do: <<fill_chr_rom>>

    game_data = trainer_bytes <> prg_data <> chr_data

    header <> game_data
  end
end
