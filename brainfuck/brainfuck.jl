type Tape
  tape::Array
  pos::Int
  inc::Function
  dec::Function
  get::Function
  advance::Function
  devance::Function

  function Tape()
    this = new()
    this.tape = Array(Int, 1)
    this.pos = 1
    this.inc = function ()
      this.tape[this.pos] += 1
    end
    this.dec = function ()
      this.tape[this.pos] -= 1
    end
    this.get = function ()
      this.tape[this.pos]
    end
    this.advance = function ()
      this.pos += 1
      if this.pos > length(this.tape)
        push!(this.tape, 0)
      end
    end
    this.devance = function ()
      if this.pos > 1
        this.pos -= 1
      end
    end
    return this
  end
end

type Program
  code::String
  bracket_map::Dict
  run::Function

  function Program(text)
    this = new()
    SYMBOLS = ['>' '<' '+' '-' '.' ',' '[' ']']
    this.code = filter(x -> in(x, SYMBOLS), text)
    this.bracket_map = Dict{Int, Int}()
    stack = Int[]
    pc = 1
    for ch in this.code
      if ch == '['
        push!(stack, pc)
      elseif ch == ']' && length(stack) > 0
        right = pop!(stack)
        this.bracket_map[pc] = right
        this.bracket_map[right] = pc
      end
      pc += 1
    end

    this.run = function ()
      tape = Tape()
      pc = 1
      while pc <= length(this.code)
        ch = this.code[pc]
        if ch == '+'
          tape.inc()
        elseif ch == '-'
          tape.dec()
        elseif ch == '>'
          tape.advance()
        elseif ch == '<'
          tape.devance()
        elseif ch == '['
          if tape.get() == 0
            pc = this.bracket_map[pc]
          end
        elseif ch == ']'
          if tape.get() != 0
            pc = this.bracket_map[pc]
          end
        elseif ch == '.'
          print(char(tape.get()))
        end
        pc += 1
      end
    end

    return this
  end
end

text = open(readall, ARGS[1])
p = Program(text)
p.run()
