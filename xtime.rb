#!/usr/bin/env ruby
def mem(pid); `ps p #{pid} -o rss`.split.last.to_i; end
t = Time.now
pid = Process.spawn(*ARGV.to_a)
mm = 0

Thread.new do
  mm = mem(pid)
  while true
    sleep 0.1
    m = mem(pid)
    mm = m if m > mm
  end
end

Process.waitall
STDERR.puts "%.2fs, %.1fMb" % [Time.now - t, mm / 1024.0]

