# frozen_string_literal: true

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
    @tape << 0 while @pos >= @tape.size
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
      when :inc then tape.inc(op.val)
      when :move then tape.move(op.val)
      when :loop then _run(op.val, tape) while tape.get.positive?
      when :print then print(tape.get.chr)
      end
    end
  end

  def parse(iterator)
    res = []
    while (c = iterator.next rescue nil)
      op = case c
           when '+' then Op.new(:inc, 1)
           when '-' then Op.new(:inc, -1)
           when '>' then Op.new(:move, 1)
           when '<' then Op.new(:move, -1)
           when '.' then Op.new(:print, 0)
           when '[' then Op.new(:loop, parse(iterator))
           when ']' then break
           end
      res << op if op
    end
    res
  end
end

def notify(msg)
  Socket.tcp('localhost', 9001) { |s| s.puts msg }
rescue SystemCallError
  # standalone usage
end

if __FILE__ == $PROGRAM_NAME
  engine = RUBY_ENGINE
  if engine == 'truffleruby'
    desc = RUBY_DESCRIPTION
    if desc.include?('Native')
      engine = 'Ruby/truffleruby'
    elsif desc.include?('JVM')
      engine = 'Ruby/truffleruby (--jvm)'
    end
  elsif engine == 'ruby' && RubyVM::MJIT.enabled?
    engine = 'Ruby (--jit)'
  end

  text = IO.read(ARGV[0])

  pid = Process.pid
  notify("#{engine}\t#{pid}")

  Program.new(text).run

  notify('stop')
end
