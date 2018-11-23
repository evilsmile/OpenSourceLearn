package main

import (
	"bytes"
	"fmt"
	"io"
	"os"
)

func main() {
	var buf bytes.Buffer
	buf.Write([]byte("Hello "))
	fmt.Fprintf(&buf, "World!\n")

	io.Copy(os.Stdout, &buf)
}
