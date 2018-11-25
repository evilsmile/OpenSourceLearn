package main

import (
	"bytes"
	"fmt"
	"os"
)

func main() {
	// 使用 io.Writer 接口实现 bytes、fmt、os数据的传递

	var buf bytes.Buffer

	buf.Write([]byte("Hello "))

	fmt.Fprint(&buf, "World!\n")

	buf.WriteTo(os.Stdout)

}
