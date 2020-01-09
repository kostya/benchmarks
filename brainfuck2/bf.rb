require 'socket'

class Op
  attr_accessor :op, :val
  def initialize(op, val)
    @op = op
    @val = val
  end
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
    while (@pos >= @tape.size)
      @tape << 0
    end
  end
end

class Program
  def initialize(code)
    @ops = parse code.chars.each
  end

  def run
    _run @ops, Tape.new
  end

private

  def _run(program, tape)
    program.each do |op|
      case op.op
        when :inc; tape.inc(op.val)
        when :move; tape.move(op.val)
        when :loop; _run(op.val, tape) while tape.get > 0
        when :print; print(tape.get.chr)
      end
    end
  end

  def parse(iterator)
    res = []
    while c = iterator.next rescue nil
      op = case c
           when '+'; Op.new(:inc, 1)
           when '-'; Op.new(:inc, -1)
           when '>'; Op.new(:move, 1)
           when '<'; Op.new(:move, -1)
           when '.'; Op.new(:print, 0)
           when '['; Op.new(:loop, parse(iterator))
           when ']'; break
           end
      res << op if op
    end
    res
  end
end

def notify(msg)
  begin
    Socket.tcp('localhost', 9001) { |s|
      s.puts msg
    }
  rescue
    # standalone usage
  end
end

engine = "#{RUBY_ENGINE}"
if engine == "truffleruby"
  desc = "#{RUBY_DESCRIPTION}"
  if desc.include?('Native')
    engine = "TruffleRuby Native"
  elsif desc.include?('JVM')
    engine = "TruffleRuby JVM"
  end
end

text = IO.read(ARGV[0])

pid = Process.pid
notify("#{engine}\t#{pid}")

Program.new(text).run

notify("stop")
