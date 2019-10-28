let _ =
  let str_size = 131072 in
  let tries = 8192 in

  let str = String.make str_size 'a' in
  let str2 = Base64.encode_exn str in
  Printf.printf "encode %s... to %s...: " (String.sub str 0 4) (String.sub str2 0 4);

  let s = ref 0 in
  let t = Unix.gettimeofday() in
  for i = 1 to tries do
    let str2 = Base64.encode_exn str in
    ignore(s := !s + String.length str2);
  done;
  Printf.printf "%d, %f\n" !s (Unix.gettimeofday() -. t);

  let str3 = Base64.decode_exn str2 in
  Printf.printf "encode %s... to %s...: " (String.sub str2 0 4) (String.sub str3 0 4);

  let s = ref 0 in
  let t = Unix.gettimeofday() in
  for i = 1 to tries do
    let str3 = Base64.decode_exn str2 in
    ignore(s := !s + String.length str3);
  done;
  Printf.printf "%d, %f\n" !s (Unix.gettimeofday() -. t);
