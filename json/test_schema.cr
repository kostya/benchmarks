require "json"
require "socket"

struct Coordinate
  include JSON::Serializable

  property x : Float64
  property y : Float64
  property z : Float64

  def initialize(@x, @y, @z)
  end
end

class Coordinates
  include JSON::Serializable

  property coordinates : Array(Coordinate)
end

def notify(msg)
  begin
    TCPSocket.open("localhost", 9001) { |s|
      s.puts msg
    }
  rescue
    # standalone usage
  end
end

def calc(text)
  coordinates = Coordinates.from_json(text).coordinates
  len = coordinates.size
  x = y = z = 0

  coordinates.each do |e|
    x += e.x
    y += e.y
    z += e.z
  end

  Coordinate.new(x / len, y / len, z / len)
end

class EntryPoint
  left = calc("{\"coordinates\":[{\"x\":1.1,\"y\":2.2,\"z\":3.3}]}")
  right = Coordinate.new(1.1, 2.2, 3.3)
  if left != right
    STDERR.puts "#{left} != #{right}"
    exit(1)
  end

  text = File.read("/tmp/1.json")

  pid = Process.pid
  notify("Crystal Schema\t#{pid}")

  p calc(text)

  notify("stop")
end
