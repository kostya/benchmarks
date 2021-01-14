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
  right = Coordinate.new(2.0, 0.5, 0.25)
  ["{\"coordinates\":[{\"x\":2.0,\"y\":0.5,\"z\":0.25}]}",
   "{\"coordinates\":[{\"y\":0.5,\"x\":2.0,\"z\":0.25}]}"].each { |v|
    left = calc(v)
    if left != right
      STDERR.puts "#{left} != #{right}"
      exit(1)
    end
  }

  text = File.read("/tmp/1.json")

  notify("Crystal (Schema)\t#{Process.pid}")
  results = calc(text)
  notify("stop")

  p results
end
