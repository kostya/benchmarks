require 'json'

x = []

1000000.times do
  x << {'x' => rand, 'y' => rand, 'z' => rand, 't' => [1, 2, 3, 'something']}
end

File.open("1.json", 'w') { |f| f.write JSON.pretty_generate('coordinates' => x, 'info' => "yes") }
