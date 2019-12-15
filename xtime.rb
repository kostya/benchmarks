#!/usr/bin/env ruby
require "mkmf"
require "socket"

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

class EnergyStats
  attr_reader :has_energy_metrics

  def initialize
    @acc_e = 0
    @e = 0
    @has_energy_metrics = find_executable 'rapl-info'
    if @has_energy_metrics
      @max_e = `rapl-info -J`.to_i
    end
  end

  def update
    if @has_energy_metrics
      new_e = `rapl-info -j`.to_i
      if @e == 0
        # first reading
        @acc_e = 0
      elsif new_e > @e
        @acc_e += new_e - @e
      elsif new_e < @e
        # counter has been reset
        @acc_e += @max_e - @e + new_e
      end
      @e = new_e
    end
  end

  def val
    0.000001 * @acc_e
  end
end

energy_stats = EnergyStats.new
server = TCPServer.new 9001
pid = Process.spawn(*ARGV.to_a)

client = server.accept
test_name = client.gets.strip
puts test_name

t = Process.clock_gettime(Process::CLOCK_MONOTONIC)
mm = 0

Thread.new do
  mm = mem(pid)
  energy_stats.update
  while true
    sleep 0.1
    m = mem(pid)
    mm = m if m > mm
    energy_stats.update
  end
end

Process.waitpid(pid, 0)
energy_stats.update
t_diff = Process.clock_gettime(Process::CLOCK_MONOTONIC) - t
mm_mb = mm / 1024.0
stats = "%.2f s, %.1f Mb" % [t_diff, mm_mb]
if energy_stats.has_energy_metrics
  stats += ", %.1f J" % [energy_stats.val]
  open('results.log', 'a') { |f|
    f.puts "%s\t%f\t%f\t%f" % [test_name, t_diff, mm_mb, energy_stats.val]
  }
end
STDERR.puts stats
exit($?.exitstatus || 0)
