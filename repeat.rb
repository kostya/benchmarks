#!/usr/bin/env ruby
def mem(pid); `ps p #{pid} -o rss`.split.last.to_i; end

mm = 0
pid = 0
Thread.new do
  while true
    sleep 0.1
    m = mem(pid)
    mm = m if m > mm
  end
end

min_t = nil
10.times do
  start = Time.now
  pid = Process.spawn(*ARGV.to_a)
  Process.waitall
  t = Time.now - start
  min_t = t if min_t.nil? || t < min_t
  STDERR.puts "%.2fs" % t
end

STDERR.puts
STDERR.puts "MIN TIME: %.2fs" % min_t
STDERR.puts "PEAK MEM: %.1fMb" % (mm / 1024.0)
