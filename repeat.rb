#!/usr/bin/env ruby
# frozen_string_literal: true

def mem(pid)
  `ps p #{pid} -o rss`.split.last.to_i
end

mm = 0
pid = 0
Thread.new do
  loop do
    sleep 0.1
    m = mem(pid)
    mm = m if m > mm
  end
end

min_t = nil
10.times do
  start = Process.clock_gettime(Process::CLOCK_MONOTONIC)
  pid = Process.spawn(*ARGV.to_a)
  Process.waitall
  t = Process.clock_gettime(Process::CLOCK_MONOTONIC) - start
  min_t = t if min_t.nil? || t < min_t
  warn "#{t.round(2)}s"
end

warn
warn "MIN TIME: #{min_t.round(2)}s"
warn "PEAK MEM: #{(mm / 1024.0).round(1)}Mb"
