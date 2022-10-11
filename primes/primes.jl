using Sockets


const UPPER_BOUND = 5_000_000
const PREFIX = 32_338


mutable struct Node
    children::Dict{Char,Node}
    terminal::Bool
end
Node(children::Dict) = Node(children, false)
Node() = Node(Dict(), false)


mutable struct Sieve
    limit::Int64
    prime::BitVector

    function Sieve(limit::Int64)
        return new(limit, falses(limit + 1))
    end
end


function to_list(s::Sieve)::Vector{Int64}
    result = [2, 3]
    @inbounds for p = 1:s.limit+1
        if s.prime[p] == true
            push!(result, p)
        end
    end
    return result
end


function omit_squares!(s::Sieve)
    r = 5
    while r^2 < s.limit
        @inbounds if s.prime[r]
            i = r^2
            while i < s.limit
                s.prime[i] = false
                i += r^2
            end
        end
        r += 1
    end
end


function step1!(s::Sieve, x::Int64, y::Int64)
    n = 4x^2 + y^2
    if n <= s.limit && (mod(n, 12) in (1, 5))
        @inbounds s.prime[n] = !s.prime[n]
    end
end


function step2!(s::Sieve, x::Int64, y::Int64)
    n = 3x^2 + y^2
    if n <= s.limit && mod(n, 12) == 7
        @inbounds s.prime[n] = !s.prime[n]
    end
end


function step3!(s::Sieve, x::Int64, y::Int64)
    n = 3x^2 - y^2
    if x > y && n <= s.limit && mod(n, 12) == 11
        @inbounds s.prime[n] = !s.prime[n]
    end
end


function loop_y!(s::Sieve, x::Int64)
    y = 1
    while y^2 < s.limit
        step1!(s, x, y)
        step2!(s, x, y)
        step3!(s, x, y)
        y += 1
    end
end


function loop_x!(s::Sieve)
    x = 1
    while x^2 < s.limit
        loop_y!(s, x)
        x += 1
    end
end


function calc(s::Sieve)
    loop_x!(s)
    omit_squares!(s)
    return s
end


function generate_trie(prime_list::Vector{Int64})::Node
    root = Node()
    for p in prime_list
        head = root
        for ch in string(p)
            if !(ch in keys(head.children))
                head.children[ch] = Node()
            end
            head = head.children[ch]
        end
        head.terminal = true
    end
    return root
end


function find(upper_bound::Int64, search_prefix::Int64)::Vector
    primes = calc(Sieve(upper_bound))
    str_prefix = string(search_prefix)

    head = generate_trie(to_list(primes))

    for char in str_prefix
        head = get(head.children, char, nothing)
        head == nothing && return nothing
    end

    queue = [(head, str_prefix)]
    result = Int64[]

    while !isempty(queue)
        top, prefix = pop!(queue)

        if top.terminal == true
            push!(result, parse(Int64, prefix))
        end

        for (char, v) in top.children
            pushfirst!(queue, (v, prefix * char))
        end
    end

    sort!(result)
    return result
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
    left = [2, 23, 29]
    right = find(100, 2)

    if left != right
        println("$left != $right")
        exit()
    end
end


if abspath(PROGRAM_FILE) == @__FILE__
    verify()

    notify("Julia\t$(getpid())")
    results = find(UPPER_BOUND, PREFIX)
    notify("stop")

    println(results)
end
