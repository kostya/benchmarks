using Sockets

function matgen(n, seed)
    tmp = seed / n / n
    [tmp * (i - j) * (i + j - 2) for i = 1:n, j = 1:n]
end

function calc(n)
    n = round(Int, n / 2) * 2
    a = matgen(n, 1.0)
    b = matgen(n, 2.0)
    c = a * b
    c[Int(n / 2)+1, Int(n / 2)+1]
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

    # Assuming openblas64, may not work for all environments
    num_threads = ccall((:openblas_get_num_threads64_, Base.libblas_name), Int32, ())
    notify("Julia (threads: $(num_threads))\t$(getpid())")

    t = time()
    results = calc(n)
    elapsed = time() - t

    notify("stop")

    println(results)
    println("time: $(elapsed) s")
end
