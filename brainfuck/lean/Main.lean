import Std.Internal.UV.TCP
import Std.Internal
open Std.Internal.UV.TCP
open Std.Internal.IO

abbrev N := Nat -- Element type abstracted out, in case `UInt32` is ever faster than `Nat`

inductive Op where | inc | dec | left | right | print | loop (ops : Array Op)
structure Tape where (content : Array N) (pos : Fin content.size)

@[inline] def Tape.current : Tape → N | ⟨bs, i⟩ => bs[i]

@[inline] private def change (δ : N → N): Tape → Tape
| ⟨bs, i⟩ => ⟨bs.set! i (δ bs[i]), ⟨i.val, by simp⟩⟩
@[inline] def inc := change (· + 1)
@[inline] def dec := change (· - 1)

@[inline] def left : Tape → Tape
| ⟨bs, i⟩ => ⟨bs, ⟨i.val - 1, by omega⟩⟩

@[inline] def right : Tape → Tape
| ⟨bs, i⟩ => let i' := i.val + 1
             ⟨if i' < bs.size then bs else bs.push 0, ⟨i', by grind⟩⟩

partial
def parse (cs : List Char) : Array Op :=
  let rec loop : List Char → Array Op → List Char × Array Op
      | [], acc => ⟨[], acc⟩
      | c :: cs, acc => match c with
        | '+' => loop cs (acc.push .inc)
        | '-' => loop cs (acc.push .dec)
        | '>' => loop cs (acc.push .right)
        | '<' => loop cs (acc.push .left)
        | '.' => loop cs (acc.push .print)
        | '[' => let ⟨cs', body⟩ := loop cs #[]
                 loop cs' (acc.push (.loop body))
        | ']' => ⟨cs, acc⟩
        | _ => loop cs acc
  loop cs #[] |>.snd

/-- Generic execution environment, with operator for printing character parameterized -/
class Ctx (m : Type → Type) extends Monad m where
  write : N → m Unit

/-- IO as execution environment for benchmarking, flushing each character -/
instance : Ctx IO where
  write n := do let stdout ← IO.getStdout
                stdout.putStr [Char.ofNat n].asString
                stdout.flush

def checkSum : N × N → N
| ⟨s₁, s₂⟩ => s₂ * 256 + s₁

def accCheckSum : N → N × N → N × N
| n, ⟨s₁, s₂⟩ => let s₁' := (s₁ + n).mod 255
                 let s₂' := (s₁' + s₂).mod 255
                 ⟨s₁', s₂'⟩

/-- Checksuming execution environment accumulates the state being the checksum -/
instance : Ctx (StateM (N × N)) where
  write := modify ∘ accCheckSum

partial
def run [Ctx m] (ops : Array Op) (i : Nat) (t : Tape) : m Tape :=
  if _ : i < ops.size
  then match ops[i] with
       | .inc => run ops (i + 1) (inc t)
       | .dec => run ops (i + 1) (dec t)
       | .left => run ops (i + 1) (left t)
       | .right => run ops (i + 1) (right t)
       | .print => do Ctx.write t.current
                      run ops (i + 1) t
       | .loop body =>
         let rec loop (t : Tape) : m Tape :=
           match t.current with
           | 0 => run ops (i + 1) t
           | _ => do loop (← run body 0 t)
         loop t
  else pure t

def runFresh [Ctx m] (ops : Array Op) : m Tape := run ops 0 ⟨#[0], ⟨0, by simp⟩⟩

def runQuiet (op : Array Op) : N × N := runFresh (m := StateM (N × N)) op |>.run ⟨0, 0⟩ |>.snd

-- Test case verified at compile time
def actual :=
  let src := "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."
  let ops := parse src.data
  runQuiet ops
def expected := "Hello World!\n".foldl (λ cs c => accCheckSum c.val.toNat cs) ⟨0, 0⟩
#guard actual = expected

def notify (msg : String) : IO Unit := do
  let skt ← Socket.new
  let _ := (← skt.connect (.v4 ⟨⟨⟨#[127, 0, 0, 1], rfl⟩⟩, 9001⟩)) |>.result? |>.get
  let _ := (← skt.send msg.toUTF8) |>.result? |>.get
  let _ := (← skt.shutdown) |>.result? |>.get

def main : List String → IO UInt32
| [fn] => do
    let src ← IO.FS.readFile fn
    let quiet ← IO.getEnv "QUIET"
    notify s!"Lean\t {(← Process.getId).toUInt64}"
    let ops := parse src.data
    match quiet with
    | .some _ => do
      let cs := runQuiet ops
      notify "stop"
      IO.println s!"Output checksum: {checkSum cs}"
    | .none => do
      let _ ← runFresh ops
      notify "stop"
    return 0
| _ => do
  IO.println "Exactly one argument expected for the benchmark file"
  return 1
