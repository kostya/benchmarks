package main

import "encoding/base64"
import "fmt"
import "time"
import "strings"

func main() {
  STR_SIZE := 10000000
  TRIES := 100

  str := strings.Repeat("a", STR_SIZE)
  str2 := ""
  bytes := []byte(str)

  coder := base64.StdEncoding

  t := time.Now()
  s := uint64(0)

  for i := 0; i < TRIES; i += 1 {
    str2 = coder.EncodeToString(bytes)
    s += uint64(len(str2))
  }
  fmt.Printf("encode: %d, %.4f\n", s, float32(time.Since(t).Seconds()))

  t = time.Now()
  s = 0
  for i := 0; i < TRIES; i += 1 {
    str3, _ := coder.DecodeString(str2)
    s += uint64(len(str3))
  }
  fmt.Printf("decode: %d, %.4f\n", s, float32(time.Since(t).Seconds()))
}
