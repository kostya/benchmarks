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
    use Bitwise
    (p.sum2 <<< 8) ||| p.sum1
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
      |> BF.run(%BF.Tape{}, %Printer{quiet: true}), 1))

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
    p = elem(BF.parse(text)
      |> BF.run(%BF.Tape{}, %Printer{quiet: System.get_env("QUIET")}), 1)
    notify("stop")

    if p.quiet do
      IO.puts("Output checksum: #{Printer.get_checksum(p)}")
    end
  end
end

Benchmark.run()
