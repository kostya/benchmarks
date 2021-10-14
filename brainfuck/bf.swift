// Written by Ricardo Silva Veloso; distributed under the MIT license

import Foundation
import Glibc

enum Op {
    case dec
    case inc
    case prev
    case next
    case print
    case loop([Op])
}

struct Tape {
    var pos = 0
    var tape: ContiguousArray<Int32> = [0]
    var currentCell: Int32 {
        get { tape[pos] }
        set { tape[pos] = newValue }
    }

    mutating func dec() {
        currentCell -= 1
    }

    mutating func inc() {
        currentCell += 1
    }

    mutating func prev() {
        pos -= 1
    }

    mutating func next() {
        pos += 1
        if pos >= tape.count {
            tape.append(contentsOf: repeatElement(0, count: tape.capacity * 2))
        }
    }
}

class Printer {
    var sum1: Int32 = 0
    var sum2: Int32 = 0
    var quiet: Bool = false
    var checksum: Int32 {
        get { (sum2 << 8) | sum1 }
    }

    init(quiet: Bool) {
        self.quiet = quiet
    }

    func print(_ n: Int32) {
        if quiet {
            sum1 = (sum1 + n) % 255
            sum2 = (sum2 + sum1) % 255
        } else {
            putc(n, stdout)
        }
    }
}

@main
class Program {
    let ops: [Op]
    let p: Printer

    init(code: String, p: Printer) {
        var it = code.makeIterator()
        self.ops = Program.parse(&it)
        self.p = p
    }

    static func main() throws {
        verify()
        if CommandLine.argc < 2 {
            exit(EXIT_FAILURE)
        }
        let text = try String(contentsOfFile: CommandLine.arguments[1])
        let process = ProcessInfo.processInfo
        let p = Printer(quiet: process.environment["QUIET"] != nil)
        setbuf(stdout, nil)

        notify("Swift\t\(process.processIdentifier)")
        Program(code: text, p: p).run()
        notify("stop")

        if p.quiet {
            print("Output checksum: \(p.checksum)")
        }
    }

    static func verify() {
        let s = """
            ++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>\
            ---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.
        """
        let p_left = Printer(quiet: true)
        Program(code: s, p: p_left).run()
        let left = p_left.checksum

        let p_right = Printer(quiet: true)
        for c in "Hello World!\n" {
            p_right.print(Int32(c.asciiValue!))
        }
        let right = p_right.checksum

        if left != right {
            fputs("\(left) != \(right)", stderr)
            exit(EXIT_FAILURE)
        }
    }

    private static func parse<I>(_ it: inout I) -> [Op]
    where
        I: IteratorProtocol,
        I.Element == Character
    {
        var buf: [Op] = []
        loop: while let c = it.next() {
            switch c {
            case "-":
                buf.append(.dec)
            case "+":
                buf.append(.inc)
            case "<":
                buf.append(.prev)
            case ">":
                buf.append(.next)
            case ".":
                buf.append(.print)
            case "[":
                buf.append(.loop(parse(&it)))
            case "]":
                break loop
            default:
                continue
            }
        }
        return buf
    }

    func run() {
        var tape = Tape()
        _run(ops, &tape)
    }

    private func _run(_ program: [Op], _ tape: inout Tape) {
        for op in program {
            switch op {
            case .dec:
                tape.dec()
            case .inc:
                tape.inc()
            case .prev:
                tape.prev()
            case .next:
                tape.next()
            case .print:
                p.print(tape.currentCell)
            case let .loop(program):
                while tape.currentCell > 0 {
                    _run(program, &tape)
                }
            }
        }
    }
}

func notify(_ msg: String) {
    let sock = socket(AF_INET, Int32(SOCK_STREAM.rawValue), 0)
    var serv_addr = (
        sa_family_t(AF_INET),
        in_port_t(htons(9001)),
        in_addr(s_addr: 0),
        (0,0,0,0,0,0,0,0))
    inet_pton(AF_INET, "127.0.0.1", &serv_addr.2)
    let len = MemoryLayout.stride(ofValue: serv_addr)
    let rc = withUnsafePointer(to: &serv_addr) { ptr -> Int32 in
        return ptr.withMemoryRebound(to: sockaddr.self, capacity: 1) {
            bptr in return connect(sock, bptr, socklen_t(len))
        }
    }
    if rc == 0 {
        msg.withCString { (cstr: UnsafePointer<Int8>) -> Void in
            send(sock, cstr, Int(strlen(cstr)), 0)
            close(sock)
        }
    }
}
