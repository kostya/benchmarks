import encoding.base64
import benchmark
import net

fn notify() {
    sock := net.dial('127.0.0.1', 9001) or {
        return
    }
    mut lang := "V GCC"
    $if clang {
      lang = "V Clang"
    }
    sock.write(lang) or {}
    sock.close() or {}
}

fn main() {
  str_size := 131072
  tries := 8192

  str := 'a'.repeat(str_size)
  notify()

  str2 := base64.encode(str)
  print('encode ${str[0..4]}... to ${str2[0..4]}...: ')

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
  print('decode ${str2[0..4]}... to ${str3[0..4]}...: ')

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
