local specialInstruction = {
	["."] = "w(data[i])",
	[","] = "data[i]=r()",
	["["] = "while data[i]~=0 do ",
	["]"] = "end "
}

local artithmeticsIns = {
	["+"] = "data[i] = data[i]+",
	["-"] = "data[i] = data[i]-",
	[">"] = "i=i+",
	["<"] = "i=i-"
}

local brainfuck = function(s)
	s = s:gsub("[^%+%-<>%.,%[%]]+", "") -- remove new lines
	local instList = {}
	local slen = #s
	local i = 2
	local lastInstruction = s:sub(1, 1)
	local arithmeticsCount = 0

	if (artithmeticsIns[lastInstruction]) then
		arithmeticsCount = 1
	end

	while (i <= slen) do
		local curInst = s:sub(i, i)

		if curInst == lastInstruction then
			if artithmeticsIns[curInst] then
				arithmeticsCount = arithmeticsCount + 1
			else
				table.insert(instList, specialInstruction[curInst])
			end
		else
			if artithmeticsIns[lastInstruction] then
				table.insert(instList, artithmeticsIns[lastInstruction] .. arithmeticsCount .. ' ')

				if artithmeticsIns[curInst] then
					arithmeticsCount = 1
				else
					table.insert(instList, specialInstruction[curInst])
					arithmeticsCount = 0
				end
			else
				if artithmeticsIns[curInst] then
					arithmeticsCount = 1
				else
					table.insert(instList, specialInstruction[curInst])
					arithmeticsCount = 0
				end
			end
		end

		lastInstruction = curInst
		i = i + 1
	end

	local code = [[local data;
if type(rawget(_G, "jit")) == 'table' then
	local ffi = require("ffi")
	data = ffi.new("int[1024]")
else
	data = {}
	local i = 0
	while i < 1024 do
		data[i] = 0
		i = i + 1
	end
end
local i = 0

local w = function(c)
	io.write(string.char(c))
end

local r = function()
	return io.read(1):byte()
end

]] .. table.concat(instList)
	local loadstring = loadstring or load

	return loadstring(code, "brainfuck", "t")
end

local function notify(msg)
	local socket = require"socket"

	socket.protect(function()
		local c = socket.try(socket.connect("localhost", 9001))

		local try = socket.newtry(function()
			c:close()
		end)

		try(c:send(msg))
		c:close()
	end)()
end

(function(arg)
	local f = io.open(arg[1])
	local text = f:read("*a")
	f:close()
	local compiler = type(jit) == 'table' and "Lua/luajit" or "Lua"
	local getpid = require'posix.unistd'.getpid
	notify(string.format("%s\t%d", compiler, getpid()))
	local brainfuckFunc = brainfuck(text)
	brainfuckFunc()
	notify("stop")
end)(arg)
