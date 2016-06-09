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
    new_pos = @pos + x
    if new_pos >= @tape.size
      (new_pos - @tape.size + 1).times { @tape << 0 }
    end
    @pos = new_pos if new_pos >= 0
  end
end

def run(program, tape)
  program.each do |op|
    case op.op
    when :inc
      tape.inc(op.val)
    when :move
      tape.move(op.val)
    when :loop
      while tape.get != 0
        run(op.val, tape)
      end
    when :print
      print(tape.get.chr)
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

iterator = File.read(ARGV[0]).chars.each
run(parse(iterator), Tape.new)
