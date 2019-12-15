require 'json'
require 'socket'

begin
  Socket.tcp('localhost', 9001) { |s|
    engine = "#{RUBY_ENGINE}"
    if engine == "truffleruby"
      desc = "#{RUBY_DESCRIPTION}"
      if desc.include?('Native')
        engine = "TruffleRuby Native"
      elsif desc.include?('JVM')
        engine = "TruffleRuby JVM"
      end
    end
    s.puts engine
  }
rescue
  # standalone usage
end

jobj = JSON.parse(File.read('1.json'))
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
