require "base64"

STR_SIZE = 131072
TRIES = 8192

str = "a" * STR_SIZE

str2 = Base64.strict_encode64(str)
print "encode #{str[0..3]}... to #{str2[0..3]}...: "

t, s = Process.clock_gettime(Process::CLOCK_MONOTONIC), 0
TRIES.times do |i|
  str2 = Base64.strict_encode64(str)
  s += str2.bytesize
end
puts "#{s}, #{Process.clock_gettime(Process::CLOCK_MONOTONIC) - t}"

str3 = Base64.strict_decode64(str2)
print "decode #{str2[0..3]}... to #{str3[0..3]}...: "

t, s = Process.clock_gettime(Process::CLOCK_MONOTONIC), 0
TRIES.times do |i|
  str3 = Base64.strict_decode64(str2)
  s += str3.bytesize
end
puts "#{s}, #{Process.clock_gettime(Process::CLOCK_MONOTONIC) - t}"
