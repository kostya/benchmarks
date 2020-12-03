# code from https://github.com/MikeInnes/BrainForth/blob/master/src/brainfuck.jl
using Sockets

mutable struct Tape
    count::Int
    pos::Int
    tape::Vector{Int}
end

Tape() = Tape(0, 1, [0])

mutable struct Printer
    sum1::Int
    sum2::Int
    quiet::Bool
end

Printer(quiet) = Printer(0, 0, quiet)

getChecksum(p::Printer) = (p.sum2 << 8) | p.sum1

Base.getindex(t::Tape) = t.tape[t.pos]
Base.setindex!(t::Tape, v) = t.tape[t.pos] = v

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

inc!(t::Tape) = t[] += 0x01
dec!(t::Tape) = t[] -= 0x01

function write!(n::Int, p::Printer)
    if p.quiet
        p.sum1 = (p.sum1 + n) % 255
        p.sum2 = (p.sum2 + p.sum1) % 255
    else
        write(stdout, n)
    end
end

function interpret(t::Tape, bf, p::Printer)
    loops = Int[]
    scan = 0
    ip = 1
    @inbounds while ip <= length(bf)
        t.count += 1
        op = bf[ip]
        if op == '['
            scan > 0 || t[] == 0 ? (scan += 1) : push!(loops, ip)
        elseif op == ']'
            scan > 0 ? (scan -= 1) : t[] == 0 ? pop!(loops) : (ip = loops[end])
        elseif scan == 0
            if op == '+'
                inc!(t)
            elseif op == '-'
                dec!(t)
            elseif op == '<'
                left!(t)
            elseif op == '>'
                right!(t)
            elseif op == '.'
                write!(t[], p)
            end
        end
        ip += 1
    end
    return t
end

function notify(msg)
    try
        socket = connect("localhost", 9001)
        write(socket, msg)
        close(socket)
    catch
        # standalone usage
    end
end

function verify()
    text = """++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>
        ---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."""
    pLeft = Printer(true)
    interpret(Tape(), collect(text), pLeft)
    left = getChecksum(pLeft)

    pRight = Printer(true)
    for c in "Hello World!\n"
        write!(convert(Int, c), pRight)
    end
    right = getChecksum(pRight)

    if left != right
        println(stderr, "$(left) != $(right)")
        exit(1)
    end
end

if abspath(PROGRAM_FILE) == @__FILE__
    verify()
    text = open(ARGS[1]) do file
        read(file, String)
    end
    p = Printer(haskey(ENV, "QUIET"))

    notify("Julia\t$(getpid())")
    interpret(Tape(), collect(text), p)
    notify("stop")

    if p.quiet
        println("Output checksum: $(getChecksum(p))")
    end
end
