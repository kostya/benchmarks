import encoding.base64
import benchmark

fn main() {
  str_size := 131072
  tries := 8192

  str := 'a'.repeat(str_size)

  str2 := base64.encode(str)
  str3 := base64.decode(str2)
  
  str2_buffer := malloc( str2.len )
  str3_buffer := malloc( str3.len )
  
  print('encode ${str.substr(0, 4)}... to ${str2.substr(0, 4)}...: ')

  mut bench := benchmark.new_benchmark()
  bench.step()
  mut s := 0
  for i := 0; i < tries; i++ {
    s += base64.encode_in_buffer(str, mut str2_buffer)
  }
  bench.ok()
  println(bench.step_message('$s'))

  print('decode ${str2.substr(0, 4)}... to ${str3.substr(0, 4)}...: ')

  bench.step()
  s = 0
  for i := 0; i < tries; i++ {    
    s += base64.decode_in_buffer( str2, mut str3_buffer )
  }
  bench.ok()
  println(bench.step_message('decode: $s'))
  
  bench.stop()
  println(bench.total_message('running base64 encode/decode ops'))
}
