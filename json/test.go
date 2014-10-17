package main
import "encoding/json"
import "fmt"
import "io/ioutil"

type Coordinate struct {
  X, Y, Z float64
}

type TestStruct struct {
  Coordinates []Coordinate
}

func main() {
  Text, err := ioutil.ReadFile("./1.json")
  if err != nil { panic(err) }

  dat := TestStruct{}
  if err := json.Unmarshal([]byte(Text), &dat); err != nil { panic(err) }

  x := 0.0
  y := 0.0
  z := 0.0

  for i := 0; i < len(dat.Coordinates); i += 1 {
    coord := dat.Coordinates[i]
    x += coord.X
    y += coord.Y
    z += coord.Z
  }

  len := float64(len(dat.Coordinates))
  fmt.Printf("%.8f\n%.8f\n%.8f\n", x / len, y / len, z / len)
}
