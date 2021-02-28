defmodule BelmontTest do
  use ExUnit.Case
  doctest Belmont

  test "greets the world" do
    assert Belmont.hello() == :world
  end
end
