package main

import (
	"fmt"
)

func main() {

	// has invalid utf8 char
	str := "\xfe\x30\x40"

	// Wrong.  \xfe treated as 0XFFFD rune（�）
	for _, c := range str {
		fmt.Printf("%#x ", c)
	}

	fmt.Println()

	// Right
	for _, c := range []byte(str) {
		fmt.Printf("%#x ", c)
	}

}
