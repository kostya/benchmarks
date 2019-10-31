#!/usr/bin/env ruby
def mem(pid)
  parent_rss = `ps p #{pid} -o rss`
  overall = parent_rss.split("\n").last.to_i
  unless RUBY_PLATFORM =~ /darwin/
    children_rss = `ps --ppid #{pid} -o rss`
    children_rss.split("\n").drop(1).each do |mem|
      overall += mem.to_i
    end
  end
  overall
end



t = Process.clock_gettime(Process::CLOCK_MONOTONIC)
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

Process.waitpid(pid, 0)
STDERR.puts "%.2fs, %.1fMb" % [Process.clock_gettime(Process::CLOCK_MONOTONIC) - t, mm / 1024.0]
exit($?.exitstatus || 0)
