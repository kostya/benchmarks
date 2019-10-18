import encoding.base64
import benchmark

fn main() {
  str_size := 10000000
  tries := 100

  str := 'a'.repeat(str_size)
  mut str2 := ''

  mut bench := benchmark.new_benchmark()
  bench.step()
  mut s := 0
  for i := 0; i < tries; i++ {
    str2 = base64.encode(str)
    s += str2.len
  }
  bench.ok()
  println(bench.step_message('encode: $s'))

  bench.step()
  s = 0
  for i := 0; i < tries; i++ {
    str3 := base64.decode(str2)
    s += str3.len
  }
  bench.ok()
  println(bench.step_message('decode: $s'))
}
