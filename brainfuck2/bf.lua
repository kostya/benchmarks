local INC = 0
local MOVE = 1
local PRINT = 3
local LOOP = 4

-- Tape class

local Tape = {}
Tape.__index = Tape

function Tape.new()
  local self = setmetatable({}, Tape)
  self.data = {0}
  self.pos = 1
  return self
end

function Tape.current(self)
  return self.data[self.pos]
end

function Tape.inc(self, x)
  self.data[self.pos] = self.data[self.pos] + x
end

function Tape.move(self, x)
  local length = #self.data
  self.pos = self.pos + x
  for i = length + 1, self.pos do
    self.data[i] = 0
  end
end

-- Parser and interpreter

function parse(source, i)
  local res = {}
  while i <= #source do
    local c = source:sub(i, i)
    if     c == "+" then res[#res + 1]  = {INC, 1}
    elseif c == "-" then res[#res + 1]  = {INC, -1}
    elseif c == ">" then res[#res + 1]  = {MOVE, 1}
    elseif c == "<" then res[#res + 1]  = {MOVE, -1}
    elseif c == "." then res[#res + 1]  = {PRINT, nil}
    elseif c == "[" then
      loop_code, i = parse(source, i + 1)
      res[#res + 1]  = {LOOP, loop_code}
    elseif c == "]" then break
    end
    i = i + 1
  end
  return res, i
end

function run(program, tape)
  for i = 1, #program do
    local op = program[i]
    if     op[1] == INC then tape:inc(op[2])
    elseif op[1] == MOVE then tape:move(op[2])
    elseif op[1] == PRINT then
      io.write(string.char(tape:current()))
      io.flush()
    elseif op[1] == LOOP then
      while tape:current() ~= 0 do
        run(op[2], tape)
      end
    end
  end
end

-- Startup code

f = io.open(arg[1])
source = f:read("*a")
f:close()
program, _ = parse(source, 1)
run(program, Tape.new())
