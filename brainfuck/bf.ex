defmodule BF.Tape do
  def init() do
    {:atomics.new(30_000, []), 1, 1}
  end

  def current({data, pos, _max_pos}) do
    :atomics.get(data, pos)
  end

  def inc({data, pos, max_pos}, x) when x == -1 or x == 1 do
    :atomics.add(data, pos, x)

    {data, pos, max_pos}
  end

  def move({data, pos, max_pos}, x) when x == -1 or x == 1 do
    new_pos = pos + x

    if new_pos <= max_pos do
      {data, new_pos, max_pos}
    else
      :atomics.put(data, new_pos, 0)
      {data, new_pos, new_pos}
    end
  end
end

defmodule Printer do
  @enforce_keys [:quiet]
  defstruct [:quiet, sum1: 0, sum2: 0]

  def print(n, p) do
    if p.quiet do
      new_sum1 = rem(p.sum1 + n, 255)
      new_sum2 = rem(p.sum2 + new_sum1, 255)
      %Printer{p | sum1: new_sum1, sum2: new_sum2}
    else
      :ok = IO.binwrite([n])
      :file.sync(:stdout)
      p
    end
  end

  def get_checksum(p) do
    import Bitwise
    (p.sum2 <<< 8) ||| p.sum1
  end
end

defmodule BF do
  alias BF.Tape, as: Tape

  def parse(source) do
    {result, []} = parse(to_charlist(source), [])
    result
  end

  defp parse([], acc) do
    {Enum.reverse(acc), []}
  end

  defp parse([x | xs], acc) do
    case x do
      ?+ -> parse(xs, [{:inc, 1} | acc])
      ?- -> parse(xs, [{:inc, -1} | acc])
      ?> -> parse(xs, [{:move, 1} | acc])
      ?< -> parse(xs, [{:move, -1} | acc])
      ?. -> parse(xs, [{:print, nil} | acc])
      ?[ ->
        {loop_code, new_xs} = parse(xs, [])
        parse(new_xs, [{:loop, loop_code} | acc])
      ?] -> {Enum.reverse(acc), xs}
      _   -> parse(xs, acc)
    end
  end

  def run([], tape, p) do
    {tape, p}
  end

  def run(program = [op | ops], tape, p) do
    case op do
      {:inc, x} -> run(ops, Tape.inc(tape, x), p)
      {:move, x} -> run(ops, Tape.move(tape, x), p)
      {:print, nil} ->
        p = Printer.print(Tape.current(tape), p)
        run(ops, tape, p)
      {:loop, loop_code} ->
        if Tape.current(tape) == 0 do
          run(ops, tape, p)
        else
          {tape, p} = run(loop_code, tape, p)
          run(program, tape, p)
        end
    end
  end
end

defmodule Benchmark do
  def notify(msg) do
    with {:ok, socket} <- :gen_tcp.connect('localhost', 9001, []) do
      :gen_tcp.send(socket, msg)
      :gen_tcp.close(socket)
    end
  end

  def verify do
    text = """
    ++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>
    ---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.
    """
    left = Printer.get_checksum(
      elem(BF.parse(text)
      |> BF.run(BF.Tape.init(), %Printer{quiet: true}), 1))

    right = Printer.get_checksum(
      Enum.reduce(
        String.to_charlist("Hello World!\n"),
        %Printer{quiet: true},
        &Printer.print/2))
    if left != right do
      IO.puts(:stderr, "#{left} != #{right}")
      exit(:shutdown)
    end
  end

  def run do
    verify()
    [filename] = System.argv()
    text = File.read!(filename)

    notify("Elixir\t#{System.pid()}")

    p = text
      |> BF.parse()
      |> BF.run(BF.Tape.init(), %Printer{quiet: System.get_env("QUIET")})
      |> elem(1)

    notify("stop")

    if p.quiet do
      IO.puts("Output checksum: #{Printer.get_checksum(p)}")
    end
  end
end

Benchmark.run()
