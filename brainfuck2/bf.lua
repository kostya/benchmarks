local INC = 0
local MOVE = 1
local PRINT = 3
local LOOP = 4

local function Tape()
  local pos = 1;
  local data = {0};

  local get = function()
    return data[pos];
  end

  local inc = function(x)
    data[pos] = data[pos] + x;
  end

  local move = function(x)
    local length = #data;
    pos = pos + x;
    for i = length + 1, pos do
      data[i] = 0;
    end
  end

  return {
    get = get;
    inc = inc;
    move = move;
  };
end

local function Brainfuck(text)
  local function parse(source, i)
    local res = {};
    while i <= source:len() do
      local c = source:sub(i, i);
      if c == "+" then
        table.insert(res, {INC, 1});
      elseif c == "-" then
        table.insert(res, {INC, -1});
      elseif c == ">" then
        table.insert(res, {MOVE, 1});
      elseif c == "<" then
        table.insert(res, {MOVE, -1});
      elseif c == "." then
        table.insert(res, {PRINT});
      elseif c == "[" then
        local loop_code;
        loop_code, i = parse(source, i+1);
        table.insert(res, {LOOP, loop_code});
      elseif c == "]" then
        break;
      end
      i = i + 1;
    end
    return res, i;
  end

  local function _run(program, tape)
    for i = 1, #program do
      local op = program[i];
      local operator = op[1];
      if operator == INC then
        tape.inc(op[2]);
      elseif operator == MOVE then
        tape.move(op[2]);
      elseif operator == LOOP then
        while tape.get() > 0 do
          _run(op[2], tape);
        end
      elseif operator == PRINT then
        io.write(string.char(tape.get()));
        io.flush();
      end
    end
  end

  local function run()
    _run(parse(text, 1), Tape())
  end

  return {
    run = run;
  };
end

local socket = require "socket"
socket.protect(function()
  local c = socket.try(socket.connect("localhost", 9001))
  local try = socket.newtry(function() c:close() end)
  try(c:send(type(jit) == 'table' and "LuaJIT" or "Lua"))
  c:close()
end)()

local text = io.open(arg[1]):read("*a");
local brainfuck = Brainfuck(text);
brainfuck.run();