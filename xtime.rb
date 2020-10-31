#!/usr/bin/env ruby
# frozen_string_literal: true

require 'fileutils'
require 'socket'

PAGE_SIZE = `getconf PAGESIZE`.to_i
HAS_MEM = File.file?('/proc/self/statm')
RESULTS_LOG = 'target/results.log'

def read_mem(pid)
  if HAS_MEM
    begin
      stat = IO.read("/proc/#{pid}/statm").split
      PAGE_SIZE * stat[1].to_i # man 5 proc
    rescue Errno::ENOENT
      0
    end
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
spawned_pid = Process.spawn(*ARGV.to_a)

begin
  client = server.accept_nonblock
rescue IO::WaitReadable
  exit(1) unless Process.waitpid(spawned_pid, Process::WNOHANG).nil?
  IO.select([server], nil, nil, 1)
  retry
end

test_data = client.gets.strip.split("\t")
test_name = test_data[0]
pid = test_data[1].to_i
p test_name

t = Process.clock_gettime(Process::CLOCK_MONOTONIC, :nanosecond)
mem = read_mem(pid)
base_mem = mem
energy_stats.update
while IO.select([server], nil, nil, 0.01).nil?
  m = read_mem(pid)
  mem = m if m > mem
  energy_stats.update
end
client = server.accept
client.gets

energy_stats.update
t_diff = Process.clock_gettime(Process::CLOCK_MONOTONIC, :nanosecond) - t
stats = "#{(t_diff / 1e9).round(3)} s, #{(mem / 1_048_576).round(1)} Mb"
FileUtils.mkdir_p File.dirname(RESULTS_LOG)

if energy_stats.has_energy_metrics
  stats += ", #{energy_stats.val.round(1)} J"
  File.open(RESULTS_LOG, 'a') do |f|
    f.puts "#{test_name}\t#{t_diff}\t#{base_mem}\t#{mem}\t#{energy_stats.val}"
  end
else
  stats += ', 0.0 J'
  File.open(RESULTS_LOG, 'a') do |f|
    f.puts "#{test_name}\t#{t_diff}\t#{base_mem}\t#{mem}\t0.0"
  end
end

Process.wait spawned_pid
warn stats
