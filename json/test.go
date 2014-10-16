package main
import "encoding/json"
import "fmt"
import "io/ioutil"
import "os"

type Coordinate struct {
    X, Y, Z float64
    t interface{}
}

type TestStruct struct {
    Info string
    Coordinates []Coordinate
}

func FileRead(filename string) (result string, err error) {
  file, err := os.Open(filename)
  if err != nil { return }

  Code, err := ioutil.ReadAll(file)
  if err != nil { return }

  result = string(Code)
  return
}

func main() {
  Text, err := FileRead("./1.json")
  if err != nil { panic(err) }

  dat := TestStruct{}
  if err := json.Unmarshal([]byte(Text), &dat); err != nil { panic(err) }

  len := len(dat.Coordinates)
  x := 0.0
  y := 0.0
  z := 0.0

  for i := 0; i < len; i += 1 {
    coord := dat.Coordinates[i]
    x += coord.X
    y += coord.Y
    z += coord.Z
  }

  fmt.Printf("%.4f\n%.4f\n%.4f\n", x, y, z)
}
