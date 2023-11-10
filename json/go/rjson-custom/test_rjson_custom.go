package main

import (
	"benchmark"
	"github.com/willabides/rjson"
)

type handler struct {
	buffer rjson.Buffer
	coord  benchmark.Coordinate
	count  float64 // used to calculate the running average
}

func (h *handler) HandleArrayValue(data []byte) (p int, err error) {
	h.count++
	return rjson.HandleObjectValues(data, h, &h.buffer)
}

// HandleObjectValue handles both the top level object where we only care about the "coordinates" field
// and the object inside the array where we keep a running average of x, y, and z values.
func (h *handler) HandleObjectValue(fieldname, data []byte) (int, error) {
	var ptr *float64 // points to X, Y or Z depending on the fieldname
	switch string(fieldname) {
	case "x":
		ptr = &h.coord.X
	case "y":
		ptr = &h.coord.Y
	case "z":
		ptr = &h.coord.Z
	case "coordinates":
		return rjson.HandleArrayValues(data, h, &h.buffer)
	default:
		return rjson.SkipValueFast(data, &h.buffer)
	}
	val, p, err := rjson.ReadFloat64(data)
	if err != nil {
		return p, err
	}
	*ptr += val
	return p, nil
}

func calc(bytes []byte) (benchmark.Coordinate, error) {
	h := &handler{}
	_, err := rjson.HandleObjectValues(bytes, h, &h.buffer)
	if err != nil {
		return benchmark.Coordinate{}, err
	}
	return benchmark.Coordinate{
		X: h.coord.X / h.count,
		Y: h.coord.Y / h.count,
		Z: h.coord.Z / h.count,
	}, nil
}

func main() {
	err := benchmark.Run("rjson custom", calc)
	if err != nil {
		panic(err)
	}
}
