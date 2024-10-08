using LinearAlgebra
using Sockets

function matgen(n, seed)
    tmp = seed / n / n
    [tmp * (i - j) * (i + j - 2) for i = 1:n, j = 1:n]
end

function calc(n)
    n = n ÷ 2 * 2
    a = matgen(n, 1)
    b = matgen(n, 2)
    c = a * b
    c[n÷2+1, n÷2+1]
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

if abspath(PROGRAM_FILE) == @__FILE__
    n = length(ARGS) > 0 ? parse(Int, ARGS[1]) : 100

    left = calc(101)
    right = -18.67
    if abs(left - right) > 0.1
        println(stderr, "$(left) != $(right)")
        exit(1)
    end

    num_threads = LinearAlgebra.BLAS.get_num_threads()
    notify("Julia (threads: $(num_threads))\t$(getpid())")

    t = time()
    results = calc(n)
    elapsed = time() - t

    notify("stop")

    println(results)
    println("time: $(elapsed) s")
end
