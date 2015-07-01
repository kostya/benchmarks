import structs/[ArrayList, HashMap, Stack]
import io/File

Tape: class {
    tape := ArrayList<Int> new()
    pos := 0

    init: func { tape add(0) }

    get: func -> Int { tape[pos] }
    inc: func { tape[pos] = tape[pos] + 1 }
    dec: func { tape[pos] = tape[pos] - 1 }
    advance: func {
        pos += 1
        if (pos >= tape size) {
            tape add(0)
        }
    }
    devance: func { pos -= 1 }
}

Program: class {
    code := ArrayList<Char> new()
    bracketMap := HashMap<Int, Int> new()
    init: func (text: String) {
        leftstack := Stack<Int> new()
        pc := 0
        valid := ['[', ']', '<', '>', '+', '-', ',', '.'] as ArrayList<Char>
        for (chr in text) {
            if (valid contains?(chr)) {
                code add(chr)
                match chr {
                    case '[' =>
                        leftstack push(pc)
                    case ']' =>
                        left := leftstack pop()
                        right := pc
                        bracketMap[left] = right
                        bracketMap[right] = left
                }
                pc += 1
            }
        }
    }

    run: func {
        pc := 0
        tape := Tape new()
        while (pc < code size) {
            chr := code[pc]
            match chr {
                case '+' => tape inc()
                case '-' => tape dec()
                case '>' => tape advance()
                case '<' => tape devance()
                case '[' => if (tape get() == 0) { pc = bracketMap[pc] }
                case ']' => if (tape get() != 0) { pc = bracketMap[pc] }
                case '.' => tape get() as Char print(); stdout flush()
            }
            pc += 1
        }
    }
}

main: func(args: ArrayList<String>) {
    Program new(File new(args get(1)) read()) run()
}
