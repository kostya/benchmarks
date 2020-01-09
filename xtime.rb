#!/usr/bin/env ruby
require "socket"

$page_size = `getconf PAGESIZE`.to_i
$has_mem = File.file?("/proc/self/statm")

def mem(pid)
  if $has_mem
    stat = IO.read("/proc/#{pid}/statm").split
    $page_size * stat[1].to_i # man 5 proc
  else
    0
  end
end

class EnergyStats
  PATH = "/sys/class/powercap/intel-rapl/intel-rapl:0/energy_uj"

  attr_reader :has_energy_metrics

  def initialize
    @acc_e = 0
    @e = 0
    @has_energy_metrics = File.file?(PATH)
    if @has_energy_metrics
      @max_e = IO.read(PATH).to_i
    end
  end

  def update
    if @has_energy_metrics
      new_e = IO.read(PATH).to_i
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
Process.spawn(*ARGV.to_a)

client = server.accept
test_data = client.gets.strip.split("\t")
test_name = test_data[0]
pid = test_data[1].to_i
puts test_name

t = Process.clock_gettime(Process::CLOCK_MONOTONIC)
mm = mem(pid)
energy_stats.update
while IO.select([server], nil, nil, 0.01).nil?
  m = mem(pid)
  mm = m if m > mm
  energy_stats.update
end

energy_stats.update
t_diff = Process.clock_gettime(Process::CLOCK_MONOTONIC) - t
mm_mb = mm / 1048576.0
stats = "%.2f s, %.1f Mb" % [t_diff, mm_mb]
if energy_stats.has_energy_metrics
  stats += ", %.1f J" % [energy_stats.val]
  open('results.log', 'a') { |f|
    f.puts "%s\t%f\t%f\t%f" % [test_name, t_diff, mm_mb, energy_stats.val]
  }
end
STDERR.puts stats
