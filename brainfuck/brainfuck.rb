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
        @chars << char.ord
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
    len = @chars.length
    while pc < len
      case @chars[pc]
        when 43; tape.inc
        when 45; tape.dec
        when 62; tape.advance
        when 60; tape.devance
        when 91; pc = @bracket_map[pc] if tape.get == 0
        when 93; pc = @bracket_map[pc] if tape.get != 0
        when 46; print(tape.get.chr)
      end
      pc += 1
    end
  end
end

text = File.read(ARGV[0])
Program.new(text).run
