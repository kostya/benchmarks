import strformat, strutils, sequtils
import net
import tables
import posix
import deques


const UPPER_BOUND: int = 5_000_000
const PREFIX: int = 32_338

type
  NodeRef = ref Node
  Node = object
    children: Table[char, NodeRef]
    terminal: bool
  Sieve = object
    limit: int
    primes: seq[bool]
  Pair = object
    node: NodeRef
    str: string

proc init(sieve: var Sieve) =
  sieve.primes = newSeqWith(sieve.limit + 1, false)


proc toList(sieve: Sieve): seq[int] =
  result = @[2, 3]
  for p in 5..sieve.limit:
    if sieve.primes[p]:
      result.add(p)

proc omitSquares(sieve: var Sieve) =
  var r: int = 5
  while r * r < sieve.limit:
    if sieve.primes[r]:
      var i: int = r * r
      while i < sieve.limit:
        sieve.primes[i] = false
        i.inc(r * r)
    r.inc()

proc step1(sieve: var Sieve, x, y: int): void =
  let n: int = (4 * x * x) + (y * y)
  if n <= sieve.limit and n mod 12 in [1, 5]:
    sieve.primes[n] = not sieve.primes[n]


proc step2(sieve: var Sieve, x, y: int): void =
  let n: int = (3 * x * x) + (y * y)
  if n <= sieve.limit and n mod 12 == 7:
    sieve.primes[n] = not sieve.primes[n]

proc step3(sieve: var Sieve, x, y: int): void =
  let n: int = (3 * x * x) - (y * y)
  if x > y and n < sieve.limit and n mod 12 == 11:
    sieve.primes[n] = not sieve.primes[n]

proc loopY(seive: var Sieve, x: int) =
  var y: int = 1
  while y*y < seive.limit:
    seive.step1(x, y)
    seive.step2(x, y)
    seive.step3(x, y)
    y.inc()

proc loopX(sieve: var Sieve) =
  var x = 1
  while x*x < sieve.limit:
    sieve.loopY(x)
    x.inc()

proc calc(sieve: var Sieve) =
  sieve.loopX()
  sieve.omitSquares()

proc generateTrie(l: seq[int]): NodeRef =
  result = NodeRef()
  for el in l:
    var head = result
    for ch in $el:
      if not head.children.hasKey(ch):
        head.children[ch] = NodeRef()
      head = head.children[ch]
    head.terminal = true

proc find(upperBound: int, prefix: int): seq[int] =
  var primes = Sieve(limit: upperBound)
  primes.init()
  primes.calc()
  var head = generateTrie(primes.toList())
  for ch in $prefix:
    head = head.children[ch]
    if head == nil:
      return @[]
  var queue = @[Pair(node: head, str: $prefix)].toDeque()

  while queue.len > 0:
    var pair = queue.popLast()
    if pair.node.terminal:
      result.add(parseInt(pair.str))
    for (ch, node) in pair.node.children.pairs():
      queue.addFirst(Pair(node: node, str: pair.str & $ch))

proc notify(msg: string) =
  let sock = net.dial("127.0.0.1", Port(9001))
  defer: sock.close()
  sock.send(msg)
  discard

proc verify()  =
  let left = @[2, 23, 29]
  let right = find(100, 2)
  try:
    assert(left == right)
  except:
    echo "Expected: ", left
    echo "Actual: ", right

when isMainModule:
  verify()

  var compiler = "Nim/clang"
  when defined(gcc):
    compiler = "Nim/gcc"
  notify(fmt"{compiler}\t{getpid()}")
  let result = find(UPPER_BOUND, PREFIX)
  notify("stop")
  echo result
