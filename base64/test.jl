using Base64
using Sockets

function main()
    str_size = 131072
    tries = 8192
    str = repeat("a", str_size)
    str2 = base64encode(str)
    str3 = String(base64decode(str2))

    notify("Julia\t$(getpid())")

    t = time()
    s_encoded = 0
    for i = 1:tries
        s_encoded += length(base64encode(str))
    end
    t_encoded = time() - t

    t = time()
    s_decoded = 0
    for i = 1:tries
        s_decoded += length(base64decode(str2))
    end
    t_decoded = time() - t

    notify("stop")

    print("encode $(str[1:4])... to $(str2[1:4]): $s_encoded, $t_encoded\n")
    print("decode $(str2[1:4])... to $(str3[1:4]): $s_decoded, $t_decoded\n")
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
    for (src, dst) in [["hello", "aGVsbG8="], ["world", "d29ybGQ="]]
        encoded = base64encode(src)
        if encoded != dst
            println(stderr, "$(encoded) != $(dst)")
            exit(1)
        end
        decoded = String(base64decode(dst))
        if decoded != src
            println(stderr, "$(decoded) != $(src)")
            exit(1)
        end
    end

    main()
end
