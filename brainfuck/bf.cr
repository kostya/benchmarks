require "socket"

module Op
  record Inc, val : Int32
  record Move, val : Int32
  record Print
  alias T = Inc | Move | Print | Array(Op::T)
end

class Tape
  def initialize
    @tape = [0]
    @pos = 0
  end

  def get
    @tape[@pos]
  end

  def inc(x)
    @tape[@pos] += x
  end

  def move(x)
    @pos += x
    while @pos >= @tape.size
      @tape << 0
    end
  end
end

class Printer
  getter quiet

  def initialize(quiet : Bool)
    @sum1 = 0
    @sum2 = 0
    @quiet = quiet
  end

  def print(n : Int32)
    if @quiet
      @sum1 = (@sum1 + n) % 255
      @sum2 = (@sum2 + @sum1) % 255
    else
      print(n.chr)
    end
  end

  def checksum
    (@sum2 << 8) | @sum1
  end
end

class Program
  @ops : Array(Op::T)

  def initialize(code : String, p : Printer)
    @ops = parse(code.each_char)
    @p = p
  end

  def run
    _run @ops, Tape.new
  end

  private def _run(program, tape)
    program.each do |op|
      case op
      when Op::Inc
        tape.inc(op.val)
      when Op::Move
        tape.move(op.val)
      when Array(Op::T)
        while tape.get > 0
          _run(op, tape)
        end
      when Op::Print
        @p.print(tape.get)
      else
        # pass
      end
    end
  end

  private def parse(iterator)
    res = [] of Op::T
    iterator.each do |c|
      op = case c
           when '+'; Op::Inc.new(1)
           when '-'; Op::Inc.new(-1)
           when '>'; Op::Move.new(1)
           when '<'; Op::Move.new(-1)
           when '.'; Op::Print.new
           when '['; parse(iterator)
           when ']'; break
	   else; # pass
           end
      res << op if op
    end
    res
  end
end

def notify(msg)
  begin
    TCPSocket.open("localhost", 9001) { |s|
      s.puts msg
    }
  rescue
    # standalone usage
  end
end

def verify
  text = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>
       ---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."
  p_left = Printer.new(true)
  Program.new(text, p_left).run
  left = p_left.checksum

  p_right = Printer.new(true)
  "Hello World!\n".each_char { |c| p_right.print(c.ord) }

  right = p_right.checksum
  if left != right
    STDERR.puts "#{left} != #{right}"
    exit(1)
  end
end

class EntryPoint
  verify
  text = File.read(ARGV[0])
  p = Printer.new(ENV.has_key?("QUIET"))

  notify("Crystal\t#{Process.pid}")
  Program.new(text, p).run
  notify("stop")

  if p.quiet
    puts "Output checksum: #{p.checksum}"
  end
end
