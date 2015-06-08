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

class Op
  attr_accessor :char, :jump
  def initialize(char, jump)
    @char = char
    @jump = jump
  end
end

class Program
  def initialize(text)
    @code = []
    text.each_char do |char|
      @code << Op.new(char, 0) if "[]<>+-,.".include?(char)
    end

    leftstack = []
    @code.each_with_index do |op, pc|
      char = op.char
      if char == '['
        leftstack << pc
      elsif char == ']' && !leftstack.empty?
        left = leftstack.pop
        right = pc
        @code[left].jump = right
        @code[right].jump = left
      end
    end
  end

  def run
    tape = Tape.new
    pc = 0
    len = @code.size
    while pc < len
      op = @code[pc]
      case op.char
        when '+'; tape.inc
        when '-'; tape.dec
        when '>'; tape.advance
        when '<'; tape.devance
        when '['; pc = op.jump if tape.get == 0
        when ']'; pc = op.jump if tape.get != 0
        when '.'; print(tape.get.chr)
      end
      pc += 1
    end
  end
end

text = File.read(ARGV[0])
Program.new(text).run
