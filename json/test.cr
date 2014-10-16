require "json"

text = File.read("1.json")
json = Json.parse(text) as Hash
puts json.length
