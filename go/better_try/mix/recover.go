package main

import (
	"fmt"
)

func panicF() (str string) {

	defer func() {
		if err := recover(); err != nil {
			str = "Recover from error!"
		}
	}()

	panic("PACNIC")
	str = "Succ"
	return
}

func main() {
	fmt.Println(panicF())
}
