local ffi = require "ffi"

local function Tape(capacity)
  capacity = capacity or 8192
  local data = ffi.new ("uint8_t[?]", capacity, 0)
  local pos = 1

  return {
    get = function () return data[pos] end,
    inc = function () data[pos] = data[pos] + 1 end,
    dec = function () data[pos] = data[pos] - 1 end,
    forth = function () pos = pos + 1 end,
    back = function () pos = pos - 1 end,
    put = function (x) data[pos] = string.byte(x) end,
  }
end

local function Printer(quiet)
  local sum1 = 0
  local sum2 = 0

  local function quiet_print(n)
      sum1 = (sum1 + n) % 255
      sum2 = (sum2 + sum1) % 255
  end

  local function print(n)
      io.write(string.char(n))
      io.flush()
  end

  local function get_checksum()
    local bit = require "bit"
    return bit.bor(bit.lshift(sum2, 8), sum1)
  end

  return {
    print = quiet and quiet_print or print,
    get_checksum = get_checksum,
    quiet = quiet
  }
end

local function Brainfuck(text, p)
  local print = p.print

  local codes = {
    ["+"] = function (tape) tape.inc() end,
    ["-"] = function (tape) tape.dec() end,
    ["<"] = function (tape) tape.back() end,
    [">"] = function (tape) tape.forth() end,
    ["."] = function (tape) print(tape.get()) end,
    [","] = function (tape) tape.put(io.read(1)) end,
  }

  local function while_loop(tape, program)
    while tape.get() > 0 do
      for i = 1, #program do
        local op = program[i]
        op(tape)
      end
    end
  end

  local function parse(source, i)
    local result = {}
    i = i or 1
    while i <= source:len() do
      local c = source:sub(i, i)
      local op = codes[c]
      if op == nil then
        if c == "]" then
          break
        elseif c == "[" then
          op, i = parse(source, i + 1)
          result[#result + 1] = function (tape) while_loop(tape, op) end
        end
      else
        result[#result + 1] = op
      end
      i = i + 1
    end
    return result, i
  end

  local function run ()
    local program = parse(text)
    local tape = Tape()
    for i = 1, #program do
      local op = program[i]
      op(tape)
    end
  end

  return {
    run = run
  }
end

local function verify()
  local text = [[
  ++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>
  ---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.
  ]]
  local p_left = Printer(true)
  Brainfuck(text, p_left).run()
  local left = p_left.get_checksum()

  local p_right = Printer(true)
  local str = "Hello World!\n"
  for i = 1, #str do
    p_right.print(string.byte(str, i))
  end
  local right = p_right.get_checksum()

  if left ~= right then
    print(string.format("%d != %d", left, right))
    os.exit(1)
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

(function(arg)
    verify()
    local f = io.open(arg[1])
    local text = f:read("*a")
    f:close()
    local p = Printer(os.getenv("QUIET"))

    local socket = Socket("localhost", 9001)
    socket.notify_version()
    Brainfuck(text, p).run()
    socket.notify("stop")
    if p.quiet then
      print(string.format("Output checksum: %d", p.get_checksum()))
    end
end)(arg)
