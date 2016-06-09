record Op, op : Symbol, val : Int32 | Array(Op)

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
      tape.inc(op.val.as(Int32))
    when :move
      tape.move(op.val.as(Int32))
    when :loop
      while tape.get != 0
        run(op.val.as(Array), tape)
      end
    when :print
      print(tape.get.chr)
    end
  end
end

def parse(iterator)
  res = [] of Op
  iterator.each do |c|
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

iterator = File.read(ARGV[0]).each_char
run(parse(iterator), Tape.new)
