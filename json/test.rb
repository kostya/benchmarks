# frozen_string_literal: true

require 'json'
require 'socket'

Coordinate = Struct.new(:x, :y, :z)

def notify(msg)
  Socket.tcp('localhost', 9001) { |s| s.puts msg }
rescue SystemCallError
  # standalone usage
end

def calc(text)
  jobj = JSON.parse(text)
  coordinates = jobj['coordinates']
  len = coordinates.length
  x = y = z = 0

  coordinates.each do |coord|
    x += coord['x']
    y += coord['y']
    z += coord['z']
  end

  Coordinate.new(x / len, y / len, z / len)
end

if __FILE__ == $PROGRAM_NAME
  right = Coordinate.new(2.0, 0.5, 0.25)
  ['{"coordinates":[{"x":2.0,"y":0.5,"z":0.25}]}',
   '{"coordinates":[{"y":0.5,"x":2.0,"z":0.25}]}'].each do |v|
    left = calc(v)
    if left != right
      warn "#{left} != #{right}"
      exit(1)
    end
  end

  text = IO.read('/tmp/1.json')

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

  notify("#{engine}\t#{Process.pid}")
  results = calc(text)
  notify('stop')

  p results
end
