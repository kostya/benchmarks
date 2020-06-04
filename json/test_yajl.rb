# frozen_string_literal: true

require 'yajl'
require 'socket'

Coordinate = Struct.new(:x, :y, :z)

def notify(msg)
  Socket.tcp('localhost', 9001) { |s| s.puts msg }
rescue SystemCallError
  # standalone usage
end

def calc(text)
  jobj = Yajl::Parser.new.parse(text)
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

left = calc('{"coordinates":[{"x":1.1,"y":2.2,"z":3.3}]}')
right = Coordinate.new(1.1, 2.2, 3.3)
if left != right
  warn "#{left} != #{right}"
  exit(1)
end

text = IO.read('/tmp/1.json')

pid = Process.pid
notify("Ruby YAJL\t#{pid}")

p calc(text)

notify('stop')
