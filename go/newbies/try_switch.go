package main

import (
	"fmt"
)

func check(c byte) bool {
	switch c {
	case ' ':
		// go through to next case
		fallthrough
	case '\t':
		return false
	}
	return true
}

func main() {
	fmt.Println(check(' '))
	fmt.Println(check('\t'))
	a := ^2
	fmt.Printf("%#x\n", a)
}
