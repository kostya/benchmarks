using Pkg
Pkg.add("JSON3")

import JSON3
using Sockets

struct Coordinate
    x::Float64
    y::Float64
    z::Float64
end

function calc(text)
    jobj = JSON3.read(text)
    coordinates = jobj["coordinates"]
    len = length(coordinates)
    x = y = z = 0

    for coord in coordinates
        x += coord["x"]
        y += coord["y"]
        z += coord["z"]
    end

    Coordinate(x / len, y / len, z / len)
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
    right = Coordinate(1.1, 2.2, 3.3)
    for v in [
        """{"coordinates":[{"x":1.1,"y":2.2,"z":3.3}]}""",
        """{"coordinates":[{"y":2.2,"x":1.1,"z":3.3}]}""",
    ]
        left = calc(v)
        if left != right
            println(stderr, "$(left) != $(right)")
            exit(1)
        end
    end

    text = open("/tmp/1.json") do file
        read(file, String)
    end

    notify("Julia (JSON3)\t$(getpid())")

    println(calc(text))

    notify("stop")
end
