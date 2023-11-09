package common

import (
	"errors"
	"fmt"
	"net"
	"os"
	"runtime"
)

// creates a TCP connection to port 9001
func createConn() (*net.TCPConn, error) {
	notifyAddr := &net.TCPAddr{Port: 9001}
	conn, err := net.DialTCP("tcp", nil, notifyAddr)
	if err != nil {
		var e *net.OpError
		if errors.As(err, &e) && e.Op == "dial" {
			return nil, nil
		}
		return nil, err
	}
	return conn, nil
}

func benchName(sub string) string {
	name := "Go"
	if runtime.Compiler != "gc" {
		name += "/" + runtime.Compiler
	}
	if sub != "" {
		name += " (" + sub + ")"
	}
	return name
}

// RunBenchmark runs the benchmark function and notifies xtime of the start and stop.
func RunBenchmark(name string, benchmark func()) error {
	startConn, err := createConn()
	if err != nil {
		return err
	}
	stopConn, err := createConn()
	if err != nil {
		return err
	}
	startMsg := []byte(fmt.Sprintf("%s\t%d\n", benchName(name), os.Getpid()))
	stopMsg := []byte("stop")

	if startConn != nil {
		_, err = startConn.Write(startMsg)
		if err != nil {
			return err
		}
		err = startConn.Close()
		if err != nil {
			return err
		}
	}

	benchmark()

	if stopConn != nil {
		_, err = stopConn.Write(stopMsg)
		if err != nil {
			return err
		}
		err = stopConn.Close()
		if err != nil {
			return err
		}
	}
	return nil
}
