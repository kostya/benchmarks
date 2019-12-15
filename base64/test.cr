require "base64"
require "socket"

STR_SIZE = 131072
TRIES = 8192

str = "a" * STR_SIZE

begin
  TCPSocket.open("localhost", 9001) { |s|
    s.puts "Crystal"
  }
rescue
  # standalone usage
end

str2 = Base64.strict_encode(str)
print "encode #{str[0..3]}... to #{str2[0..3]}...: "

t, s = Time.local, 0
TRIES.times do |i|
  str2 = Base64.strict_encode(str)
  s += str2.bytesize
end
puts "#{s}, #{Time.local - t}"

str3 = Base64.decode_string(str2)
print "decode #{str2[0..3]}... to #{str3[0..3]}...: "

t, s = Time.local, 0
TRIES.times do |i|
  str3 = Base64.decode_string(str2)
  s += str3.bytesize
end
puts "#{s}, #{Time.local - t}"
