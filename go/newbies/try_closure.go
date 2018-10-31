package main

import (
	"fmt"
	"strings"
)

func handleSuffix(suffix string) func(string) string {

	return func(str string) string {
		if strings.HasSuffix(str, suffix) == false {
			return str + suffix
		}
		return str
	}
}

// go tool vet -shadow try_closure.go
func checkShadow() {
	x := 1
	fmt.Println("x overlapped:%d", x)
	{
		x := 2
		fmt.Println("x overlapped:%d", x)
	}
	x = 3

}

func main() {
	f := handleSuffix(".bmp")

	fmt.Println(f("hasNoSuffix"))
	fmt.Println(f("hasSuffix.bmp"))
}
