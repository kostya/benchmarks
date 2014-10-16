require "json"

res = {x: 0.0, y: 0.0, z: 0.0}
count = 0

File.open("1.json") do |file|
  io = BufferedIO.new(file)

  pull = Json::PullParser.new(io)
  pull.on_key!("coordinates") do
    pull.read_array do
      count += 1
      pull.read_object do |key|
        case key
        when "x" then res[:x] += pull.read_float
        when "y" then res[:y] += pull.read_float
        when "z" then res[:z] += pull.read_float
        else                     pull.skip
        end
      end
    end
  end
end

p res[:x] / count
p res[:y] / count
p res[:z] / count
