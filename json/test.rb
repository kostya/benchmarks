# frozen_string_literal: true

require 'json'
require 'socket'

def notify(msg)
  Socket.tcp('localhost', 9001) { |s| s.puts msg }
rescue SystemCallError
  # standalone usage
end

engine = RUBY_ENGINE
if engine == 'truffleruby'
  desc = RUBY_DESCRIPTION
  if desc.include?('Native')
    engine = 'TruffleRuby Native'
  elsif desc.include?('JVM')
    engine = 'TruffleRuby JVM'
  end
end

text = IO.read('/tmp/1.json')

pid = Process.pid
notify("#{engine}\t#{pid}")

jobj = JSON.parse(text)
coordinates = jobj['coordinates']
len = coordinates.length
x = y = z = 0

coordinates.each do |coord|
  x += coord['x']
  y += coord['y']
  z += coord['z']
end

p x / len
p y / len
p z / len

notify('stop')
