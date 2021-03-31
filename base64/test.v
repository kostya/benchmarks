import encoding.base64
import net
import time

fn notify(msg string) {
	mut sock := net.dial_tcp('127.0.0.1:9001') or { return }
	defer {
		sock.close() or {}
	}
	sock.write_string(msg) or {}
}

fn main() {
	for fixture in [
		['hello', 'aGVsbG8='],
		['world', 'd29ybGQ='],
	] {
		src := fixture[0]
		dst := fixture[1]
		encoded := base64.encode(src.bytes())
		if encoded != dst {
			panic('$encoded != $dst')
		}
		decoded := base64.decode(dst)
		if decoded != src.bytes() {
			panic('$decoded != $src')
		}
	}
	str_size := 131072
	tries := 8192
	str := 'a'.repeat(str_size).bytes()
	str2 := base64.encode(str)
	str3 := base64.decode(str2)
	mut lang := 'V/gcc'
	$if clang {
		lang = 'V/clang'
	}
	notify('$lang\t$C.getpid()')
	mut sw := time.new_stopwatch({})
	mut s_encoded := 0
	for i := 0; i < tries; i++ {
		str2_local := base64.encode(str)
		s_encoded += str2_local.len
		unsafe {
			str2_local.free()
		}
	}
	t_encoded := sw.elapsed().seconds()
	sw.restart()
	mut s_decoded := 0
	for i := 0; i < tries; i++ {
		str3_local := base64.decode(str2)
		s_decoded += str3_local.len
		unsafe {
			str3_local.free()
		}
	}
	t_decoded := sw.elapsed().seconds()
	notify('stop')
	println('encode ${str[0..4]}... to ${str2[0..4]}...: $s_encoded, $t_encoded')
	println('decode ${str2[0..4]}... to ${str3[0..4]}...: $s_decoded, $t_decoded')
	unsafe {
		str2.free()
		str3.free()
	}
}
