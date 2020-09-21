# frozen_string_literal: true

require 'base64'
require 'socket'

def notify(msg)
  Socket.tcp('localhost', 9001) { |s| s.puts msg }
rescue SystemCallError
  # standalone usage
end

if __FILE__ == $PROGRAM_NAME
  STR_SIZE = 131_072
  TRIES = 8192

  str = 'a' * STR_SIZE

  engine = RUBY_ENGINE
  if engine == 'truffleruby'
    desc = RUBY_DESCRIPTION
    if desc.include?('Native')
      engine = 'Ruby/truffleruby'
    elsif desc.include?('JVM')
      engine = 'Ruby/truffleruby (--jvm)'
    end
  elsif engine == 'ruby' && RubyVM::MJIT.enabled?
    engine = 'Ruby (--jit)'
  end

  pid = Process.pid
  notify("#{engine}\t#{pid}")

  t = Process.clock_gettime(Process::CLOCK_MONOTONIC)
  s = 0

  str2 = Base64.strict_encode64(str)
  print "encode #{str[0..3]}... to #{str2[0..3]}...: "

  TRIES.times do
    str2 = Base64.strict_encode64(str)
    s += str2.bytesize
  end
  puts "#{s}, #{Process.clock_gettime(Process::CLOCK_MONOTONIC) - t}"

  str3 = Base64.strict_decode64(str2)
  print "decode #{str2[0..3]}... to #{str3[0..3]}...: "

  t = Process.clock_gettime(Process::CLOCK_MONOTONIC)
  s = 0
  TRIES.times do
    str3 = Base64.strict_decode64(str2)
    s += str3.bytesize
  end
  puts "#{s}, #{Process.clock_gettime(Process::CLOCK_MONOTONIC) - t}"

  notify('stop')
end
