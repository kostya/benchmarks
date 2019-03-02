require "base64"

STR_SIZE = 10_000_000
TRIES = 100

str = "a" * STR_SIZE
str2 = ""

print "encode: "
t, s = Process.clock_gettime(Process::CLOCK_MONOTONIC), 0
TRIES.times do |i|
  str2 = Base64.strict_encode64(str)
  s += str2.bytesize
end
puts "#{s}, #{Process.clock_gettime(Process::CLOCK_MONOTONIC) - t}"

print "decode: "
t, s = Process.clock_gettime(Process::CLOCK_MONOTONIC), 0
TRIES.times do |i|
  s += Base64.strict_decode64(str2).bytesize
end
puts "#{s}, #{Process.clock_gettime(Process::CLOCK_MONOTONIC) - t}"
