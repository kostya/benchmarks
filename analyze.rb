#!/usr/bin/env ruby
# coding: utf-8
# frozen_string_literal: true

require 'fileutils'

RESULTS_LOG = 'results.log'
ATTEMPTS = 10

if ARGV.length.positive?
  FileUtils.rm_f(RESULTS_LOG)

  ATTEMPTS.times do |n|
    puts "--- Iteration #{n + 1}"
    exit unless system(*ARGV)
  end
end

Row = Struct.new(:name, :secs, :mb, :joules)

name_repl = {
  'ruby' => 'Ruby',
  'jruby' => 'JRuby',
  'gccgo' => 'GCC Go',
  'gc' => 'Go',
  'CPython' => 'Python',
  'Digital Mars D' => 'DMD',
  'GNU D' => 'GDC'
}

lines = File.open(RESULTS_LOG) do |f|
  f.readlines.map do |line|
    values = line.split("\t")
    Row.new(
      name_repl.fetch(values[0], values[0]),
      values[1],
      values[2],
      values[3].strip
    )
  end
end
keys = lines.map(&:name).uniq

def sd(list)
  mean = list.inject(:+) / list.length.to_f
  var_sum = list.map { |n| (n - mean)**2 }.inject(:+).to_f
  sample_variance = list.length > 1 ? var_sum / (list.length - 1) : 0
  format('%.2<mean>f ± %05.2<dev>f',
         mean: mean, dev: Math.sqrt(sample_variance))
end

results = keys.map do |k|
  rows = lines.select { |line| line.name == k }
  abort("Integrity check failed (#{k})") if rows.length != ATTEMPTS
  secs = sd(rows.map { |row| row.secs.to_f })
  mb = sd(rows.map { |row| row.mb.to_f })
  joules = sd(rows.map { |row| row.joules.to_f })
  Row.new(k, secs, mb, joules)
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
