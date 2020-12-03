#!/usr/bin/env ruby
# coding: utf-8
# frozen_string_literal: true

require 'fileutils'

RESULTS_LOG = 'target/results.log'
ATTEMPTS = ENV.fetch('ATTEMPTS', 10).to_i

if ARGV.length.positive?
  FileUtils.rm_f(RESULTS_LOG)

  ATTEMPTS.times do |n|
    puts "--- Iteration #{n + 1}"
    ENV['QUIET'] = '1'
    exit unless system(*ARGV)
  end
end

Row = Struct.new(:name, :secs, :base, :mb, :joules)
FinalRow = Struct.new(:name, :secs, :mb, :joules)

name_repl = {
  'ruby' => 'Ruby',
  'jruby' => 'Ruby/jruby',
  'gccgo' => 'Go/gccgo',
  'gc' => 'Go',
  'CPython' => 'Python',
  'PyPy' => 'Python/pypy',
  'Digital Mars D' => 'D/dmd',
  'GNU D' => 'D/gdc',
  'LDC' => 'D/ldc2'
}

lines = File.open(RESULTS_LOG) do |f|
  f.readlines.map do |line|
    values = line.split("\t")
    Row.new(
      name_repl.fetch(values[0], values[0]),
      values[1],
      values[2],
      values[3],
      values[4].strip
    )
  end
end
keys = lines.map(&:name).uniq

def median(array)
  return nil if array.empty?
  sorted = array.sort
  len = sorted.length
  (sorted[(len - 1) / 2] + sorted[len / 2]) / 2.0
end

def sd(list, scale=1, precision=2)
  list_median = median(list)
  deviations = list.map { |x| (x - list_median).abs }
  mad = median(deviations)
  format("%.#{precision}<median>f<sub>±%05.#{precision}<mad>f</sub>",
         median: list_median / scale, mad: mad / scale)
end

results = keys.map do |k|
  rows = lines.select { |line| line.name == k }
  abort("Integrity check failed (#{k})") if rows.length != ATTEMPTS
  secs = sd(rows.map { |row| row.secs.to_f }, 1e9, 3)
  base = sd(rows.map { |row| row.base.to_f }, 1_048_576)
  mb = sd(rows.map { |row| row.mb.to_f - row.base.to_f }, 1_048_576)
  joules = sd(rows.map { |row| row.joules.to_f })
  FinalRow.new(k, secs, "#{base} + #{mb}", joules)
end

results.sort! { |a, b| [a.secs.to_f, a.mb.to_f] <=> [b.secs.to_f, b.mb.to_f] }

max_name_len = 0
max_secs_len = 0
max_mb_len = 0
max_joules_len = 0
results.each do |row|
  name_len = row.name.length
  max_name_len = name_len if name_len > max_name_len

  secs_len = row.secs.length
  max_secs_len = secs_len if secs_len > max_secs_len

  mb_len = row.mb.length
  max_mb_len = mb_len if mb_len > max_mb_len

  joules_len = row.joules.length
  max_joules_len = joules_len if joules_len > max_joules_len
end

def pad(n, str, padstr)
  str.strip.rjust(n, padstr)
end

name_pad = ->(str, padstr = ' ') { pad(max_name_len, str, padstr) }
secs_pad = ->(str, padstr = ' ') { pad(max_secs_len, str, padstr) }
mb_pad = ->(str, padstr = ' ') { pad(max_mb_len, str, padstr) }
joules_pad = ->(str, padstr = ' ') { pad(max_joules_len, str, padstr) }
table = [
  "| #{name_pad.call('Language')} | #{secs_pad.call('Time, s')} |" \
    " #{mb_pad.call('Memory, MiB')} | #{joules_pad.call('Energy, J')} |",
  "| :#{name_pad.call('-', '-')[0...-1]} |" \
    " #{secs_pad.call('-', '-')[0...-1]}: |" \
    " #{mb_pad.call('-', '-')[0...-1]}: |" \
    " #{joules_pad.call('-', '-')[0...-1]}: |"
]

results.each do |row|
  table << "| #{name_pad.call(row.name)} | #{secs_pad.call(row.secs)} |" \
           " #{mb_pad.call(row.mb)} | #{joules_pad.call(row.joules)} |"
end

puts table.join("\n")
