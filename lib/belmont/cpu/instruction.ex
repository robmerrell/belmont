defmodule Belmont.CPU.Instruction do
  defmacro definstr(call, do: expr) do
    {name, _meta, [_cpu, opcode, operand_size]} = call
    mnemonic = name |> Atom.to_string() |> String.upcase()

    quote do
      def log(cpu, unquote(opcode)), do: log_state(cpu, unquote(opcode), unquote(mnemonic), unquote(operand_size))

      defp execute(var!(cpu), unquote(opcode)) do
        unquote(expr)
      end
    end
  end
end
