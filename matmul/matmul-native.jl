using Sockets

function matgen(n)
    tmp = 1.0 / n / n
    [ tmp * (i - j) * (i + j - 2) for i=1:n, j=1:n ]
end

function calc(n)
    n = round(Int, n / 2) * 2
    a = matgen(n)
    b = matgen(n)
    c = a * b
    c[Int(n / 2) + 1, Int(n / 2) + 1]
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

function test()
    n = length(ARGS) > 0 ? parse(Int, ARGS[1]) : 100

    left = calc(101)
    right = -9.34
    if abs(left - right) > 0.1
        println(stderr, "$(left) != $(right)")
        exit(1)
    end

    # Assuming openblas64, may not work for all environments
    num_threads = ccall(
        (:openblas_get_num_threads64_, Base.libblas_name), Int32, ())
    notify("Julia (threads: $(num_threads))\t$(getpid())")

    t = time()
    println(calc(n))
    println("time: $(time() - t) s")

    notify("stop")
end

test()
