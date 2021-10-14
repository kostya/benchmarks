package.cpath = package.cpath .. ';../common/libnotify/target/?.so'
local has_libnotify, libnotify = pcall(require, 'lua_libnotify')

local UPPER_BOUND = 5000000
local PREFIX = 32338

local function Node()
  return {
    children = {},
    terminal = false,
  }
end

local function Sieve(limit)
  local fill_array = function(size, el)
    a = {}
    for i = 1, size do
      a[i] = el
    end
    return a
  end

  local prime = fill_array(limit + 1, false)

  local to_list = function()
    local result = {2, 3}
    for p = 5, limit do
      if prime[p] then
        table.insert(result, p)
      end
    end
    return result
  end

  local omit_squares = function()
    local r = 5
    while r * r < limit do
      if prime[r] then
        local i = r * r
        while i < limit do
          prime[i] = false
           i = i + r * r
        end
      end
      r = r + 1
    end
  end

  local step1 = function(x, y)
    local n = (4 * x * x) + (y * y)
    if n <= limit and (n % 12 == 1 or n % 12 == 5) then
      prime[n] = not prime[n]
    end
  end

  local step2 = function(x, y)
    local n = (3 * x * x) + (y * y)
    if n <= limit and n % 12 == 7 then
      prime[n] = not prime[n]
    end
  end

  local step3 = function(x, y)
    local n = (3 * x * x) - (y * y)
    if x > y and n <= limit and n % 12 == 11 then
      prime[n] = not prime[n]
    end
  end

  local loop_y = function(x)
    local y = 1
    while y * y < limit do
      step1(x, y)
      step2(x, y)
      step3(x, y)
      y = y + 1
    end
  end

  local loop_x = function()
    local x = 1
    while x * x < limit do
      loop_y(x)
      x = x + 1
    end
  end

  local calc = function()
    loop_x()
    omit_squares()
  end

  return {
    calc = calc,
    to_list = to_list,
  }
end

local function Queue(list)
  list.first = 1
  list.last = #list

  local push = function(value)
    local last = list.last + 1
    list.last = last
    list[last] = value
  end

  local pop = function()
    local first = list.first
    if first > list.last then
      error("list is empty")
    end
    local value = list[first]
    list[first] = nil
    list.first = first + 1
    return value
  end

  local empty = function()
    return list.first > list.last
  end

  return {
    push = push,
    pop = pop,
    empty = empty,
  }
end

local function generate_trie(l)
  local root = Node()
  for _, el in ipairs(l) do
    local head = root
    for ch in tostring(el):gmatch"." do
      if not head.children[ch] then
        head.children[ch] = Node()
      end
      head = head.children[ch]
      end
    head.terminal = true
  end
  return root
end

local function dump(arr)
  table.sort(arr)
  return "[" .. table.concat(arr, ", ") .. "]"
end

local function find(upper_bound, prefix)
  local primes = Sieve(upper_bound)
  primes.calc()
  local str_prefix = tostring(prefix)
  local head = generate_trie(primes.to_list())

  for ch in str_prefix:gmatch"." do
    head = head.children[ch]
    if not head then
      return nil
    end
  end

  local queue, result = Queue({{head, str_prefix}}), {}
  while not queue.empty() do
    local top, prefix = table.unpack(queue.pop())
    if top.terminal then
      table.insert(result, tonumber(prefix))
    end
    for ch, v in pairs(top.children) do
      queue.push({v, prefix .. ch})
    end
  end
  return result
end

local function verify()
  local left = dump({2, 23, 29})
  local right = dump(find(100, 2))
  if left ~= right then
    io.stderr:write(string.format("%s != %s", left, right))
    os.exit(false)
  end
end

(function()
    verify()

    local compiler = type(jit) == 'table' and "Lua/luajit" or "Lua"
    if has_libnotify then libnotify.notify_with_pid(compiler) end
    local results = find(UPPER_BOUND, PREFIX)
    if has_libnotify then libnotify.notify("stop") end

    print(dump(results))
end)()
