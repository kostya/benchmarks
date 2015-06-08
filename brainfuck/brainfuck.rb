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

class Program
  def initialize(text)
    @code = []
    @maps = []

    text.each_char do |char|
      if "[]<>+-,.".include?(char)
        @code << char
        @maps << 0
      end
    end

    leftstack = []
    @code.each_with_index do |char, pc|
      char = char
      if char == '['
        leftstack << pc
      elsif char == ']' && !leftstack.empty?
        left = leftstack.pop
        right = pc
        @maps[left] = right
        @maps[right] = left
      end
    end
  end

  def run
    tape = Tape.new
    pc = 0
    len = @code.size
    while pc < len
      case @code[pc]
        when '+'; tape.inc
        when '-'; tape.dec
        when '>'; tape.advance
        when '<'; tape.devance
        when '['; pc = @maps[pc] if tape.get == 0
        when ']'; pc = @maps[pc] if tape.get != 0
        when '.'; print(tape.get.chr)
      end
      pc += 1
    end
  end
end

text = File.read(ARGV[0])
Program.new(text).run
