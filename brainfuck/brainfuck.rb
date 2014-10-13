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
    @chars = []
    @bracket_map = {}
    leftstack = []
    pc = 0
    text.each_char do |char|
      if "[]<>+-,.".include?(char)
        @chars << char
        if char == '['
          leftstack << pc
        elsif char == ']' && !leftstack.empty?
          left = leftstack.pop
          right = pc
          @bracket_map[left] = right
          @bracket_map[right] = left
        end
        pc += 1
      end
    end
  end

  def run
    tape = Tape.new
    pc = 0
    while pc < @chars.length
      case @chars[pc]
        when '+'; tape.inc
        when '-'; tape.dec
        when '>'; tape.advance
        when '<'; tape.devance
        when '['; pc = @bracket_map[pc] if tape.get == 0
        when ']'; pc = @bracket_map[pc] if tape.get != 0
        when '.'; print(tape.get.chr)
      end
      pc += 1
    end
  end
end

text = File.read(ARGV[0])
Program.new(text).run
