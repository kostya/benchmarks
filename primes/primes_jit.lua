local ffi = require "ffi"

local UPPER_BOUND = 5000000
local PREFIX = 32338

local function Node()
  return {
    children = {},
    terminal = false,
  }
end

local function Sieve(limit)
  local prime = ffi.new("bool[?]", limit + 1, false)

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
    return omit_squares()
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
    local str = tostring(el)
    for i = 1, #str do
      local ch = str:sub(i, i)
      if not head.children[ch] then
        head.children[ch] = Node()
      end
      head = head.children[ch]
    end
    head.terminal = true
  end
  return root
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
    local top, prefix = unpack(queue.pop())
    if top.terminal then
      table.insert(result, tonumber(prefix))
    end
    for ch, v in pairs(top.children) do
      queue.push({v, prefix .. ch})
    end
  end
  return result
end

local function dump(arr)
  table.sort(arr)
  return "[" .. table.concat(arr, ", ") .. "]"
end

local function verify()
  local left = dump({2, 23, 29})
  local right = dump(find(100, 2))
  if left ~= right then
    io.stderr:write(string.format("%s != %s", left, right))
    os.exit(false)
  end
end

local function Socket(host, port)
  ffi.cdef[[
    typedef int SOCKET;
    struct in_addr {
        uint32_t s_addr;
    };
    struct sockaddr_in {
        short   sin_family;
        uint16_t sin_port;
        struct  in_addr sin_addr;
        char    sin_zero[8];
    };
    SOCKET socket(int af, int type, int protocol);
    uint16_t htons(uint16_t hostshort);
    int inet_pton(int af, const char *restrict src, void *restrict dst);
    int connect(SOCKET s, struct sockaddr *name, int namelen);
    ssize_t send(int sockfd, const void *buf, size_t len);
    int close(int fildes);

    int getpid(void);
  ]]
  local AF_INET = 2
  local SOCK_STREAM = 1
  local C = ffi.C

  local function open_socket()
    local socket = C.socket(AF_INET, SOCK_STREAM, 0)
    local addr = ffi.new ("struct sockaddr_in", AF_INET, C.htons( port ) )
    C.inet_pton(AF_INET, host, addr.sin_addr)
    local ok = C.connect(socket, ffi.cast("struct sockaddr*", addr), ffi.sizeof(addr)) == 0
    return socket, ok
  end

  local function notify (msg)
    local socket, ok = open_socket()
    if ok then
      C.send(socket, msg, string.len(msg))
      C.close(socket)
    end
  end

  local function notify_version()
    notify(string.format("%s\t%d", "Lua/luajit", C.getpid()))
  end

  return {
    notify = notify,
    notify_version = notify_version,
  }
end

(function()
    verify()

    local compiler = type(jit) == 'table' and "Lua/luajit" or "Lua"
    local socket = Socket("localhost", 9001)
    socket.notify_version()
    local results = find(UPPER_BOUND, PREFIX)
    socket.notify("stop")

    print(dump(results))
end)()
