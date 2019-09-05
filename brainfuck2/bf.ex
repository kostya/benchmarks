
defmodule BrainFuck do

  @code """
  >++[<+++++++++++++>-]<[[>+>+<<-]>[<+>-]++++++++
  [>++++++++<-]>.[-]<<>++++++++++[>++++++++++[>++
  ++++++++[>++++++++++[>++++++++++[>++++++++++[>+
  +++++++++[-]<-]<-]<-]<-]<-]<-]<-]++++++++++.
  """

  def code() do
    @code
  end
  defmacro inc(n) do
    {:inc, n}
  end
  defmacro move(n)  do
    {:move, n}
  end
  defmacro print do
    {:print, :_}
  end
  def loop(rest) do
    {rest, acc} = parse_main(rest)
    {{:loop, acc}, rest}
  end

  def parse("+"<>rest), do: {inc(1), rest}
  def parse("-"<>rest), do: {inc(-1), rest}
  def parse(">"<>rest), do: {move(1), rest}
  def parse("<"<>rest), do: {move(-1), rest}
  def parse("."<>rest), do: {print(), rest}
  def parse("["<>rest), do: loop(rest)
  def parse("]"<>rest), do: {:break, rest}
  def parse(""), do: {:break, ""}
  def parse(rest) do
    parse(String.replace_prefix(rest, String.first(rest), ""))
    rescue _ ->
      {:break, ""}
  end

  def parse_main(str, acc \\ []) do
    case parse(str) do
      {:break, rest} -> {rest, Enum.reverse(acc)}
      {data, rest} ->
        parse_main(rest, [data | acc])
    end
  end

  def main() do
    {_, acc} = parse_main(@code)
    acc
  end

  def run() do
    BrainFuck.Ets.new()
    BrainFuck.EtsExec.main(main())
  end

  defmodule Ets do
    import :ets

    def new() do
      new(:state, [:named_table, :public, :set])
    end

    def new_row(pos) do
      insert_new(:state, {pos, 0})
    end

    def inc(pos, n) do
      update_counter(:state, pos, n)
    end

    def fetch(pos) do
      case lookup(:state, pos) do
        [] -> nil
        [{_key, v}] -> v
      end
    end

  end

  defmodule Proc do
    def start_link do
      {:ok, pid} = Agent.start_link(fn ->
        0
      end, [])
      pid
    end

    def inc(pid, n) do
      Agent.update(pid, & &1 + n)
    end

    def fetch(pid) do
      Agent.get(pid, & &1)
    end
  end


  defmodule ProcExec do
    import BrainFuck.Proc

    def get_value(tape, pos) do
      fetch(get_pid(tape, pos))
    end
    def get_pid(tape, pos) do
      Enum.at(tape, pos)
    end

    # inst for instruction
    def main(inst, tape \\ [], pos \\ 0)
    def main(inst, [], pos) do
      pid = start_link()
      main(inst, [pid], pos)
    end
    def main(whole = [h | rest], tape, pos) do
      {inst, tape, pos} =
        case h do
          {:inc, n} ->
            inc(get_pid(tape, pos), n)
            {rest, tape, pos}
          {:move, n} ->
            new_pos = pos + n
            new_tape =
              case Enum.at(tape, new_pos) do
                nil ->
                  pid = start_link()
                  List.insert_at(tape, new_pos, pid)
                _ ->
                  tape
              end
              {rest, new_tape, new_pos}
          {:print, _} ->
            :ok = IO.binwrite([get_value(tape, pos)])
            :file.sync(:stdout)
            {rest, tape, pos}
          {:loop, loop_code} ->
            case get_value(tape, pos) do
              0 -> {rest, tape, pos}
              _ ->
                {tape, pos} = main(loop_code, tape, pos)
                {whole, tape, pos}
            end
        end

      main(inst, tape, pos)
    end
    def main(_, tape, pos) do
      {tape, pos}
    end

  end


  defmodule EtsExec do
    import BrainFuck.Ets

    # inst for instruction
    def main(inst, pos \\ 0)

    def main(whole = [h | rest], pos) do
      {inst, new_pos} =
        case h do
          {:inc, n} ->
            inc(pos, n)
            {rest, pos}
          {:move, n} ->
            new_row(pos + n)
            {rest, pos + n}
          {:print, _} ->
            :ok = IO.binwrite([fetch(pos)])
            :file.sync(:stdout)
            {rest, pos}
          {:loop, loop_code} ->
            case fetch(pos) do
              0 -> {rest, pos}
              _ ->
                pos = main(loop_code, pos)
                {whole, pos}
            end
        end

        main(inst, new_pos)
    end
    def main(_, pos) do
      pos
    end

  end

  defmodule FullyEtsExec do
    import :ets

    def tables() do
      new(:pos, [:named_table, :set])
      insert_new(:pos, {0, 0})
      new(:state, [:named_table, :set])
    end

    def new_row(pos) do
      insert_new(:state, {pos, 0})
    end

    def inc(pos, n) do
      update_counter(:state, pos, n)
    end

    def fetch() do
      [{_, pos}] = lookup(:pos, 0)
      case lookup(:state, pos) do
        [] -> nil
        [{_key, v}] -> v
      end
    end

    def main(whole = [h | rest]) do
      x =
        case h do
          {:inc, n} ->
            [{_, pos}] = lookup(:pos, 0)
            update_counter(:state, pos, n)
          {:move, n} ->
            pos = update_counter(:pos, 0, n)
            insert_new(:state, {pos, 0})
          {:print, _} ->
            :ok = IO.binwrite([fetch()])
            :file.sync(:stdout)
          {:loop, loop_code} ->
            case fetch() do
              0 -> :ok
              _ ->
                main(loop_code)
                :whole
            end
        end
      case x do
        :whole -> main(whole)
        _ -> main(rest)
      end
    end
    def main(_, pos) do
      pos
    end
  end

end

defmodule BF do
  def run do
    BrainFuck.run()
  end
end
