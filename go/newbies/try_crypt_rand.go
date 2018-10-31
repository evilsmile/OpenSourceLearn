package main

import (
	"crypto/rand"
	"fmt"
)

func main() {
	b := make([]byte, 20)
	fmt.Println(b)

	_, err := rand.Read(b)
	if err != nil {
		fmt.Println(err.Error())
		return
	}
	fmt.Println(b)
}
