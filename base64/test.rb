# frozen_string_literal: true

require 'base64'
require 'socket'

def notify(msg)
  Socket.tcp('localhost', 9001) { |s| s.puts msg }
rescue SystemCallError
  # standalone usage
end

if __FILE__ == $PROGRAM_NAME
  [['hello', 'aGVsbG8='], ['world', 'd29ybGQ=']].each do |(src, dst)|
    encoded = Base64.strict_encode64(src)
    if encoded != dst
      warn "#{encoded} != #{dst}"
      exit(1)
    end
    decoded = Base64.strict_decode64(dst)
    if decoded != src
      warn "#{decoded} != #{src}"
      exit(1)
    end
  end

  STR_SIZE = 131_072
  TRIES = 8192
  str = 'a' * STR_SIZE
  str2 = Base64.strict_encode64(str)
  str3 = Base64.strict_decode64(str2)

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

  notify("#{engine}\t#{Process.pid}")

  t = Process.clock_gettime(Process::CLOCK_MONOTONIC)
  s_encoded = 0
  TRIES.times do
    s_encoded += Base64.strict_encode64(str).bytesize
  end
  t_encoded = Process.clock_gettime(Process::CLOCK_MONOTONIC) - t

  t = Process.clock_gettime(Process::CLOCK_MONOTONIC)
  s_decoded = 0
  TRIES.times do
    s_decoded += Base64.strict_decode64(str2).bytesize
  end
  t_decoded = Process.clock_gettime(Process::CLOCK_MONOTONIC) - t

  notify('stop')

  puts "encode #{str[0..3]}... to #{str2[0..3]}...: #{s_encoded}, #{t_encoded}"
  puts "decode #{str2[0..3]}... to #{str3[0..3]}...: #{s_decoded}, #{t_decoded}"
end
