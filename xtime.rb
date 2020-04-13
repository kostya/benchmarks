#!/usr/bin/env ruby
# frozen_string_literal: true

require 'socket'

PAGE_SIZE = `getconf PAGESIZE`.to_i
HAS_MEM = File.file?('/proc/self/statm')

def mem(pid)
  if HAS_MEM
    stat = IO.read("/proc/#{pid}/statm").split
    PAGE_SIZE * stat[1].to_i # man 5 proc
  else
    0
  end
end

# Container for energy stats
class EnergyStats
  PATH = '/sys/class/powercap/intel-rapl/intel-rapl:0/energy_uj'

  attr_reader :has_energy_metrics

  def initialize
    @acc_e = 0
    @e = 0
    @has_energy_metrics = File.file?(PATH)
    @max_e = IO.read(PATH).to_i if @has_energy_metrics
  end

  def update
    return unless @has_energy_metrics

    new_e = IO.read(PATH).to_i
    if @e.zero?
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
mm_mb = mm / 1_048_576.0
stats = "#{t_diff.round(2)} s, #{mm_mb.round(1)} Mb"
if energy_stats.has_energy_metrics
  stats += ", #{energy_stats.val.round(1)} J"
  open('results.log', 'a') do |f|
    f.puts "#{test_name}\t#{t_diff}\t#{mm_mb}\t#{energy_stats.val}"
  end
else
  stats += ', 0.0 J'
  open('results.log', 'a') do |f|
    f.puts "#{test_name}\t#{t_diff}\t#{mm_mb}\t0.0"
  end
end
warn stats
