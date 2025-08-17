# frozen_string_literal: true

require 'socket'

UPPER_BOUND = 5_000_000
PREFIX = 32_338

class Node
  attr_accessor :children, :terminal

  def initialize
    @children = {}
    @terminal = false
  end
end

class Sieve
  def initialize(limit)
    @limit = limit
    @prime = Array.new(limit + 1, false)
  end

  def to_list
    result = [2, 3]
    (5..@limit).each do |p|
      result.push(p) if @prime[p]
    end
    result
  end

  def omit_squares
    r = 5
    while r * r < @limit
      if @prime[r]
        i = r * r
        while i < @limit
          @prime[i] = false
          i += r * r
        end
      end
      r += 1
    end

    self
  end

  def step1(x, y)
    n = (4 * x * x) + (y * y)
    @prime[n] = !@prime[n] if n <= @limit && [1, 5].include?(n % 12)
  end

  def step2(x, y)
    n = (3 * x * x) + (y * y)
    @prime[n] = !@prime[n] if n <= @limit && n % 12 == 7
  end

  def step3(x, y)
    n = (3 * x * x) - (y * y)
    @prime[n] = !@prime[n] if x > y && n <= @limit && n % 12 == 11
  end

  def loop_y(x)
    y = 1
    while y * y < @limit
      step1(x, y)
      step2(x, y)
      step3(x, y)
      y += 1
    end
  end

  def loop_x
    x = 1
    while x * x < @limit
      loop_y(x)
      x += 1
    end
  end

  def calc
    loop_x
    omit_squares
  end
end

def generate_trie(l)
  root = Node.new
  l.each do |el|
    head = root
    el.to_s.each_char do |ch|
      head.children[ch] = Node.new unless head.children[ch]
      head = head.children[ch]
    end
    head.terminal = true
  end
  root
end

def find(upper_bound, prefix)
  primes = Sieve.new(upper_bound).calc
  str_prefix = prefix.to_s
  head = generate_trie(primes.to_list)
  str_prefix.each_char do |ch|
    head = head.children[ch]
    return nil if head.nil?
  end

  queue = [[head, str_prefix]]
  result = []
  until queue.empty?
    (top, prefix) = queue.pop
    result.push(prefix.to_i) if top.terminal
    top.children.each { |ch, v| queue.insert(0, [v, prefix + ch]) }
  end
  result.sort!
  result
end

def notify(msg)
  Socket.tcp('0.0.0.0', 9001) { |s| s.puts msg }
rescue SystemCallError
  # standalone usage
end

def verify
  left = [2, 23, 29]
  right = find(100, 2)

  return unless left != right

  warn "#{left} != #{right}"
  exit(1)
end

if __FILE__ == $PROGRAM_NAME
  verify

  engine = RUBY_ENGINE
  if engine == 'truffleruby'
    desc = RUBY_DESCRIPTION
    if desc.include?('Native')
      engine = 'Ruby/truffleruby'
    elsif desc.include?('JVM')
      engine = 'Ruby/truffleruby (JVM)'
    end
  elsif engine == 'ruby' && RubyVM::RJIT.enabled?
    engine = 'Ruby (--jit)'
  end

  notify("#{engine}\t#{Process.pid}")
  results = find(UPPER_BOUND, PREFIX)
  notify('stop')

  puts results.inspect
end
