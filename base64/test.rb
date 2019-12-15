require "base64"
require "socket"

STR_SIZE = 131072
TRIES = 8192

str = "a" * STR_SIZE

begin
  Socket.tcp('localhost', 9001) { |s|
    engine = "#{RUBY_ENGINE}"
    if engine == "truffleruby"
      desc = "#{RUBY_DESCRIPTION}"
      if desc.include?('Native')
        engine = "TruffleRuby Native"
      elsif desc.include?('JVM')
        engine = "TruffleRuby JVM"
      end
    end
    s.puts engine
  }
rescue
  # standalone usage
end

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
