require "base64"
require "socket"

STR_SIZE = 131072
TRIES = 8192

def notify(msg)
  begin
    TCPSocket.open("localhost", 9001) { |s|
      s.puts msg
    }
  rescue
    # standalone usage
  end
end

class EntryPoint
  [["hello", "aGVsbG8="], ["world", "d29ybGQ="]].each do |(src, dst)|
    encoded = Base64.strict_encode(src)
    if encoded != dst
      STDERR.puts "#{encoded} != #{dst}"
      exit(1)
    end
    decoded = Base64.decode_string(dst)
    if decoded != src
      STDERR.puts "#{decoded} != #{src}"
      exit(1)
    end
  end

  str = "a" * STR_SIZE
  str2 = Base64.strict_encode(str)
  str3 = Base64.decode_string(str2)

  notify("Crystal\t#{Process.pid}")

  t, s_encoded = Time.local, 0
  TRIES.times do |i|
    s_encoded += Base64.strict_encode(str).bytesize
  end
  t_encoded = Time.local - t

  t, s_decoded = Time.local, 0
  TRIES.times do |i|
    s_decoded += Base64.decode_string(str2).bytesize
  end
  t_decoded = Time.local - t

  notify("stop")

  puts "encode #{str[0..3]}... to #{str2[0..3]}...: #{s_encoded}, #{t_encoded}"
  puts "decode #{str2[0..3]}... to #{str3[0..3]}...: #{s_decoded}, #{t_decoded}"
end
