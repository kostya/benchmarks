local INC = 0
local MOVE = 1
local PRINT = 3
local LOOP = 4

local function Tape()
  local pos = 1
  local data = {0}

  local get = function()
    return data[pos]
  end

  local inc = function(x)
    data[pos] = data[pos] + x
  end

  local move = function(x)
    local length = #data
    pos = pos + x
    for i = length + 1, pos do
      data[i] = 0
    end
  end

  return {
    get = get,
    inc = inc,
    move = move,
  }
end

local function Printer(quiet)
  local sum1 = 0
  local sum2 = 0

  local function print(n)
    if quiet then
      sum1 = (sum1 + n) % 255
      sum2 = (sum2 + sum1) % 255
    else
      io.write(string.char(n))
      io.flush()
    end
  end

  local function get_checksum()
    local bit32 = require "bit32"
    return bit32.bor(bit32.lshift(sum2, 8), sum1)
  end

  return {
    print = print,
    get_checksum = get_checksum,
    quiet = quiet
  }
end

local function Brainfuck(text, p)
  local function parse(source, i)
    local res = {}
    while i <= source:len() do
      local c = source:sub(i, i)
      if c == "+" then
        table.insert(res, {INC, 1})
      elseif c == "-" then
        table.insert(res, {INC, -1})
      elseif c == ">" then
        table.insert(res, {MOVE, 1})
      elseif c == "<" then
        table.insert(res, {MOVE, -1})
      elseif c == "." then
        table.insert(res, {PRINT})
      elseif c == "[" then
        local loop_code
        loop_code, i = parse(source, i+1)
        table.insert(res, {LOOP, loop_code})
      elseif c == "]" then
        break;
      end
      i = i + 1
    end
    return res, i
  end

  local function _run(program, tape)
    for i = 1, #program do
      local op = program[i]
      local operator = op[1]
      if operator == INC then
        tape.inc(op[2])
      elseif operator == MOVE then
        tape.move(op[2])
      elseif operator == LOOP then
        while tape.get() > 0 do
          _run(op[2], tape)
        end
      elseif operator == PRINT then
        p.print(tape.get())
      end
    end
  end

  local function run()
    _run(parse(text, 1), Tape())
  end

  return {
    run = run
  }
end

local function notify(msg)
  local socket = require "socket"
  socket.protect(function()
    local c = socket.try(socket.connect("localhost", 9001))
    local try = socket.newtry(function() c:close() end)
    try(c:send(msg))
    c:close()
  end)()
end

local function verify()
  text = [[
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

(function(arg)
    verify()
    local f = io.open(arg[1])
    local text = f:read("*a")
    f:close()
    local p = Printer(os.getenv("QUIET"))

    local compiler = type(jit) == 'table' and "Lua/luajit" or "Lua"
    local getpid = require 'posix.unistd'.getpid
    notify(string.format("%s\t%d", compiler, getpid()))
    Brainfuck(text, p).run()
    notify("stop")

    if p.quiet then
      print(string.format("Output checksum: %d", p.get_checksum()))
    end
end)(arg)
