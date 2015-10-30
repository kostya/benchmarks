require "json"

len = 0
x = y = z = 0

File.open("1.json") do |file|
  pull = JSON::PullParser.new(file)
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
end

p x / len
p y / len
p z / len
