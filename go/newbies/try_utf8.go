package main

import (
	"fmt"
	"unicode/utf8"
)

func main() {
	fmt.Println(utf8.ValidString("ABC"))
	fmt.Println(utf8.ValidString("A\xfeBC"))
	fmt.Println(utf8.ValidString("A\\xfeBC"))
	fmt.Println(utf8.ValidString("\xe5\x9c\xa8"))
	fmt.Println("\xe5\x9c\xa8")

	c := "我们的爱"
	fmt.Printf("%s: bytesLen[%d] strLen[%d]\n", c, len(c), utf8.RuneCountInString(c))
}
