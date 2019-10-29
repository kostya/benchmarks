import encoding.base64
import benchmark

fn main() {
  str_size := 131072
  tries := 8192

  str := 'a'.repeat(str_size)

  str2 := base64.encode(str)
  print('encode ${str.substr(0, 4)}... to ${str2.substr(0, 4)}...: ')

  mut bench := benchmark.new_benchmark()
  bench.step()
  mut s := 0
  for i := 0; i < tries; i++ {
    str2_local := base64.encode(str)
    s += str2_local.len
    str2_local.free()
  }
  bench.ok()
  println(bench.step_message('$s'))

  str3 := base64.decode(str2)
  print('decode ${str2.substr(0, 4)}... to ${str3.substr(0, 4)}...: ')

  bench.step()
  s = 0
  for i := 0; i < tries; i++ {
    str3_local := base64.decode(str2)
    s += str3_local.len
    str3_local.free()
  }
  bench.ok()
  println(bench.step_message('$s'))

  str2.free()
  str3.free()
}
