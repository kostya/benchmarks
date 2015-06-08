type Tape
  tape::Array{Int, 1}
  pos::Int

  function Tape()
    new(Array(Int, 1), 1)
  end
end

inc(this::Tape) = (this.tape[this.pos] += 1)
dec(this::Tape) = (this.tape[this.pos] -= 1)
get(this::Tape) = this.tape[this.pos]

function advance(this::Tape)
  this.pos += 1
  if this.pos > length(this.tape)
    push!(this.tape, 0)
  end
  return nothing
end

function devance(this::Tape)
  if this.pos > 1
    this.pos -= 1
  end
  return nothing
end

validbfsymbol(x) = in(x, ['>', '<', '+', '-', '.', ',', '[', ']'])

type Program
  code::Array{Char}
  maps::Array{Int}

  function Program(text)
    text = filter(validbfsymbol, text)
    stack = Int[]
    code = Char[]
    maps = Int[]
    pc = 1
    for ch in text
      push!(code, ch)
      push!(maps, 0)
      if ch == '['
        push!(stack, pc)
      elseif ch == ']' && length(stack) > 0
        left = pop!(stack)
        maps[left] = pc
        maps[pc] = left
      end
      pc += 1
    end
    return new(code, maps)
  end
end

function run(this::Program)
  code = this.code
  maps = this.maps
  tape = Tape()
  pc = 1
  while pc <= length(code)
    ch = code[pc]
    if ch == '+'
      inc(tape)
    elseif ch == '-'
      dec(tape)
    elseif ch == '>'
      advance(tape)
    elseif ch == '<'
      devance(tape)
    elseif ch == '['
      if get(tape) == 0
        pc = maps[pc]
      end
    elseif ch == ']'
      if get(tape) != 0
        pc = maps[pc]
      end
    elseif ch == '.'
      print(char(get(tape)))
    end
    pc += 1
  end
end

function main()
  text = open(readall, ARGS[1])
  p = Program(text)
  run(p)
end

main()
