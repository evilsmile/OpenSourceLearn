package main

import (
	"fmt"
)

func main() {
	// recover 仅在defer中生效
	defer func() {
		fmt.Println("recover: ", recover())
	}()

	panic("not ok")
}
