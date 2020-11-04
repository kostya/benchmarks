using Sockets

function matgen(n, seed)
    tmp = seed / n / n
    [tmp * (i - j) * (i + j - 2) for i = 1:n, j = 1:n]
end

function mul(a, b)
    m, n = size(a)
    q, p = size(b)
    @assert n == q

    # transpose a for cache-friendliness
    aT = zeros(n, m)
    @simd for i = 1:m
        for j = 1:n
            @inbounds aT[j, i] = a[i, j]
        end
    end

    out = zeros(m, p)
    @simd for i = 1:m
        for j = 1:p
            z = 0.0
            for k = 1:n
                @inbounds z += aT[k, i] * b[k, j]
            end
            @inbounds out[i, j] = z
        end
    end
    out
end

function calc(n)
    n = round(Int, n / 2) * 2
    a = matgen(n, 1.0)
    b = matgen(n, 2.0)
    c = mul(a, b)
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

    notify("Julia (no BLAS)\t$(getpid())")
    t = time()
    results = calc(n)
    elapsed = time() - t
    notify("stop")

    println(results)
    println("time: $(elapsed) s")
end
