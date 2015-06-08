class Tape
  def initialize
    @tape = [0]
    @pos = 0
  end

  def get
    @tape[@pos]
  end

  def inc
    @tape[@pos] += 1
  end

  def dec
    @tape[@pos] -= 1
  end

  def advance
    @pos += 1
    @tape << 0 if @tape.size <= @pos
  end

  def devance
    @pos -= 1 if @pos > 0
  end
end

record Op, char, jump

class Program
  def initialize(text)
    @code = [] of Op
    leftstack = [] of Int32
    text.each_char { |char| @code << Op.new(char, 0) if "[]<>+-,.".includes?(char) }
    @code.each_with_index do |op, pc|
      char = op.char
      if char == '['
        leftstack << pc
      elsif char == ']' && !leftstack.empty?
        left = leftstack.pop
        right = pc
        @code[left] = Op.new(@code[left].char, right)
        @code[right] = Op.new(char, left)
      end
    end
  end

  def run
    tape = Tape.new
    pc = 0
    while pc < @code.length
      op = @code[pc]
      case op.char
        when '+'; tape.inc
        when '-'; tape.dec
        when '>'; tape.advance
        when '<'; tape.devance
        when '['; pc = op.jump if tape.get == 0
        when ']'; pc = op.jump if tape.get != 0
        when '.'; print!(tape.get.chr)
      end
      pc += 1
    end
  end
end

text = File.read(ARGV[0])
Program.new(text).run
