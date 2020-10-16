require "json"
require "socket"

struct Coordinate
  property x, y, z

  def initialize(@x : Float64, @y : Float64, @z : Float64)
  end
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
  len = 0
  x = y = z = 0

  pull = JSON::PullParser.new(text)
  pull.on_key!("coordinates") do
    pull.read_array do
      len += 1
      pull.read_object do |key|
        case key
        when "x" then x += pull.read_float
        when "y" then y += pull.read_float
        when "z" then z += pull.read_float
        else          pull.skip
        end
      end
    end
  end

  Coordinate.new(x / len, y / len, z / len)
end

class EntryPoint
  right = Coordinate.new(1.1, 2.2, 3.3)
  ["{\"coordinates\":[{\"x\":1.1,\"y\":2.2,\"z\":3.3}]}",
   "{\"coordinates\":[{\"y\":2.2,\"x\":1.1,\"z\":3.3}]}"].each { |v|
    left = calc(v)
    if left != right
      STDERR.puts "#{left} != #{right}"
      exit(1)
    end
  }

  text = File.read("/tmp/1.json")

  pid = Process.pid
  notify("Crystal (Pull)\t#{pid}")

  p calc(text)

  notify("stop")
end
