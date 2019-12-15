defmodule BF.Tape do
  alias BF.Tape, as: Tape

  defstruct data: :array.new(default: 0), pos: 0

  def current(tape) do
    :array.get(tape.pos, tape.data)
  end

  def inc(tape, x) when x == -1 or x == 1 do
    new_data = :array.set(tape.pos, :array.get(tape.pos, tape.data) + x,
                          tape.data)
    %Tape{tape | data: new_data}
  end

  def move(tape, x) when x == -1 or x == 1 do
    new_pos = tape.pos + x
    new_data =
      if new_pos < :array.size(tape.data) do
        tape.data
      else
        :array.set(new_pos, 0, tape.data)
      end
    %Tape{tape | data: new_data, pos: new_pos}
  end
end

defmodule BF do
  alias BF.Tape, as: Tape

  def parse(source) do
    {result, []} = parse(String.split(source, ""), [])
    result
  end

  defp parse([], acc) do
    {Enum.reverse(acc), []}
  end

  defp parse([x | xs], acc) do
    case x do
      "+" -> parse(xs, [{:inc, 1} | acc])
      "-" -> parse(xs, [{:inc, -1} | acc])
      ">" -> parse(xs, [{:move, 1} | acc])
      "<" -> parse(xs, [{:move, -1} | acc])
      "." -> parse(xs, [{:print, nil} | acc])
      "[" ->
        {loop_code, new_xs} = parse(xs, [])
        parse(new_xs, [{:loop, loop_code} | acc])
      "]" -> {Enum.reverse(acc), xs}
      _   -> parse(xs, acc)
    end
  end

  def run([], tape) do
    tape
  end

  def run(program = [op | ops], tape) do
    case op do
      {:inc, x} -> run(ops, Tape.inc(tape, x))
      {:move, x} -> run(ops, Tape.move(tape, x))
      {:print, nil} ->
        :ok = IO.binwrite([Tape.current(tape)])
        :file.sync(:stdout)
        run(ops, tape)
      {:loop, loop_code} ->
        if Tape.current(tape) == 0 do
          run(ops, tape)
        else
          run(program, run(loop_code, tape))
        end
    end
  end
end

defmodule Benchmark do
  def run do
    [filename] = System.argv()
    File.read!(filename)
    |> BF.parse()
    |> BF.run(%BF.Tape{})
  end
end

with {:ok, socket} <- :gen_tcp.connect('localhost', 9001, []) do
   :gen_tcp.send(socket, "Elixir")
   :gen_tcp.close(socket)
end

Benchmark.run()
