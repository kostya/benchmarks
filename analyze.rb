#!/usr/bin/env ruby
# coding: utf-8

require 'fileutils'

RESULTS_LOG = 'results.log'
ATTEMPTS = 10

if ARGV.length > 0
  FileUtils.rm_f(RESULTS_LOG)

  ATTEMPTS.times do |n|
    puts "--- Iteration #{n + 1}"
    if not system(*ARGV)
      exit
    end
  end
end

Row = Struct.new(:name, :secs, :mb, :joules)

name_repl = {
  "ruby" => "Ruby",
  "jruby" => "JRuby",
  "gccgo" => "GCC Go",
  "gc" => "Go",
  "CPython" => "Python",
  "Digital Mars D" => "DMD",
  "GNU D" => "GDC"
}

lines = File.readlines(RESULTS_LOG).map { |line|
  values = line.split("\t")
  Row.new(name_repl.fetch(values[0], values[0]), values[1], values[2], values[3].strip())
}
keys = lines.map {|row| row.name}.uniq

def sd(list)
  mean = list.inject(:+) / list.length.to_f
  var_sum = list.map {|n| (n-mean)**2 }.inject(:+).to_f
  sample_variance = list.length > 1 ? var_sum / (list.length - 1) : 0
  "%.2f ± %05.2f" % [mean, Math.sqrt(sample_variance)]
end

results = keys.map { |k|
  rows = lines.select { |line| line.name == k }
  if rows.length != ATTEMPTS
    abort("Integrity check failed")
  end
  secs = sd(rows.map { |row| row.secs.to_f })
  mb = sd(rows.map { |row| row.mb.to_f })
  joules = sd(rows.map { |row| row.joules.to_f })
  Row.new(k, secs, mb, joules)
}

results.sort! { |a, b|  [a.secs.to_f, a.mb.to_f] <=> [b.secs.to_f, b.mb.to_f] }

max_name_len = 0
max_secs_len = 0
max_mb_len = 0
max_joules_len = 0
results.each do | row |
  name_len = row.name.length
  max_name_len = name_len if name_len > max_name_len

  secs_len = row.secs.length
  max_secs_len = secs_len if secs_len > max_secs_len

  mb_len = row.mb.length
  max_mb_len = mb_len if mb_len > max_mb_len

  joules_len = row.joules.length
  max_joules_len = joules_len if joules_len > max_joules_len
end

pad = ->(n, str, padstr) { str.strip.rjust(n, padstr) }
name_pad = ->(str, padstr = " ") { pad.(max_name_len, str, padstr) }
secs_pad = ->(str, padstr = " ") { pad.(max_secs_len, str, padstr) }
mb_pad = ->(str, padstr = " ") { pad.(max_mb_len, str, padstr) }
joules_pad = ->(str, padstr = " ") { pad.(max_joules_len, str, padstr) }
table = [
  "| #{name_pad.('Language')} | #{secs_pad.('Time, s')} | #{mb_pad.('Memory, MiB')} | #{joules_pad.('Energy, J')} |",
  "| :#{  name_pad.('-', '-')[0...-1]} | #{secs_pad.('-', '-')[0...-1]}: | #{    mb_pad.('-', '-')[0...-1]}: | #{  joules_pad.('-', '-')[0...-1]}: |"
]

results.each do | row |
  table << "| #{name_pad.(row.name)} | #{secs_pad.(row.secs)} | #{mb_pad.(row.mb)} | #{joules_pad.(row.joules)} |"
end

puts table.join("\n")
