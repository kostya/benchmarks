# code from https://github.com/MikeInnes/BrainForth/blob/master/src/brainfuck.jl

mutable struct Tape
  count::Int
  pos::Int
  tape::Vector{UInt8}
end

Tape() = Tape(0, 1, [0])

Base.getindex(t::Tape) = t.tape[t.pos]
Base.setindex!(t::Tape, v) = t.tape[t.pos] = v

function Base.show(io::IO, t::Tape)
  print(io, "[$(t.count)] ")
  for i = 1:length(t.tape)
    print(io, t.tape[i], i == t.pos ? "* " : " ")
  end
end

Base.:(==)(a::Tape, b::Tape) = a.tape == b.tape

function left!(t::Tape)
  t.pos == length(t.tape) && t.tape[end] == 0 && pop!(t.tape)
  t.pos == 1 ? pushfirst!(t.tape, 0) : (t.pos -= 1)
  return
end

function right!(t::Tape)
  t.pos == 1 && t.tape[1] == 0 && (popfirst!(t.tape); t.pos -= 1)
  t.pos == length(t.tape) && push!(t.tape, 0)
  t.pos += 1
  return
end

clip(n) = n > 255 ? n - 256 : n < 0 ? n + 256 : n
inc!(t::Tape) = (t[] = clip(t[] + 1))
dec!(t::Tape) = (t[] = clip(t[] - 1))

function read!(t::Tape, io::IO)
  t[] = read(io, UInt8)
end

function write!(t::Tape, io::IO)
  write(io, t[])
end

# Gets ~370 MHz

function interpret(t::Tape, bf; input::IO = stdin, output::IO = stdout)
  loops = Int[]
  scan = 0
  ip = 1
  @inbounds while ip <= length(bf)
    t.count += 1
    op = bf[ip]
    if op == '['
      scan > 0 || t[] == 0 ? (scan += 1) :
      push!(loops, ip)
    elseif op == ']'
      scan > 0 ? (scan -= 1) :
      t[] == 0 ? pop!(loops) :
      (ip = loops[end])
    elseif scan == 0
      op == '+' ? inc!(t) :
      op == '-' ? dec!(t) :
      op == '<' ? left!(t) :
      op == '>' ? right!(t) :
      op == ',' ? read!(t, input) :
      op == '.' ? write!(t, output) :
      op == '#' ? println(t) :
        nothing
    end
    ip += 1
  end
  return t
end

interpret(t::Tape, bf::String; kws...) = interpret(t, collect(bf); kws...)

interpret(bf; kws...) = interpret(Tape(), bf; kws...)

function main(text)
  interpret(text)
end

println("JIT warming up")
main(">++[<+++++++++++++>-]<[[>+>+<<-]>[<+>-]++++++++[>++++++++<-]>[-]<<>++++++++++[>++++++++++[>++++++++++[>++++++++++[>++++++++++[>++++++++++[>++++++++++[-]<-]<-]<-]<-]<-]<-]<-]++++++++++")

println("bench")
text = open(ARGS[1]) do file
  read(file, String)
end
x = @timed main(text)
println("Elapsed: $(x[2]), Allocated: $(x[3]), GC Time: $(x[4])")

