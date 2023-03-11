// Written by Ricardo Silva Veloso; distributed under the MIT license

import Foundation

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
        get { tape.withUnsafeBufferPointer { $0[pos] } }
        set { tape.withUnsafeMutableBufferPointer { $0[pos] = newValue } }
    }

    mutating func inc(_ x: Int32) {
        currentCell &+= x
    }

    mutating func move(_ x: Int) {
        pos &+= x
        if pos >= tape.count {
            tape.append(contentsOf: repeatElement(0, count: tape.capacity * 2))
        }
    }
}

final class Printer {
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
            sum1 = (sum1 &+ n) % 255
            sum2 = (sum2 &+ sum1) % 255
        } else {
            putc(n, stdout)
        }
    }
}

@main
final class Program {
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
            fatalError("Expected argument.")
        }
        let text = try String(contentsOfFile: CommandLine.arguments[1])
        let process = ProcessInfo.processInfo
        let p = Printer(quiet: process.environment["QUIET"] != nil)
        setbuf(stdout, nil)

        notify_with_pid("Swift")
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
        var left: Int32 = 0
        do {
            let p = Printer(quiet: true)
            Program(code: s, p: p).run()
            left = p.checksum
        }
        var right: Int32 = 0
        do {
            let p = Printer(quiet: true)
            for c in "Hello World!\n" {
                p.print(Int32(c.asciiValue!))
            }
            right = p.checksum
        }
        if left != right {
            fatalError("\(left) != \(right)")
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
                tape.inc(-1)
            case .inc:
                tape.inc(1)
            case .prev:
                tape.move(-1)
            case .next:
                tape.move(1)
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
