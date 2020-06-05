using Pkg
Pkg.add("JSON3")

import JSON3
using Sockets

function main(text)
    jobj = JSON3.read(text)
    coordinates = jobj["coordinates"]
    len = length(coordinates)
    x = y = z = 0

    for coord in coordinates
        x += coord["x"]
        y += coord["y"]
        z += coord["z"]
    end

    println(x / len)
    println(y / len)
    println(z / len)
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

    text = open("/tmp/1.json") do file
        read(file, String)
    end

    notify("Julia JSON3\t$(getpid())")

    x = @timed main(text)
    println("Elapsed: $(x[2]), Allocated: $(x[3]), GC Time: $(x[4])")

    notify("stop")
end
