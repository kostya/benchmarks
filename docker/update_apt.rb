#!/usr/bin/env ruby
# frozen_string_literal: true

filename = ARGV[0]
pkgs = File.readlines(filename).map { |l| l.split('=')[0].strip }
list = pkgs.join(' ')
img = 'debian:testing'
pkg_info = `docker run --rm #{img} /bin/sh -c "apt-get update && apt-cache show #{list}"`

versions = {}
pkg = nil
pkg_info.split("\n").each do |line|
  if line.empty?
    pkg = nil
  elsif line.start_with?('Package: ')
    pkg = line.delete_prefix('Package: ')
  elsif line.start_with?('Version: ')
    version = line.delete_prefix('Version: ')
    versions[pkg] = version
  end
end

updated_pkgs = pkgs.map { |p| "#{p}=#{versions.fetch(p)}" }

File.open(filename, 'w') do |f|
  f.puts(updated_pkgs)
end
