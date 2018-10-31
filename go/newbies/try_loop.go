package main

import (
	"fmt"
	"time"
)

func testBreak() {
	var i int
loop:
	for {
		i++
		if i > 10 {
			break loop
		}
		fmt.Println("loop ", i)
	}
	fmt.Println("loop over")
}

type Person struct {
	name string
}

var (
	persons = []Person{{"tom"}, {"Mary"}, {"Jack"}}
)

// "Jack" "Jack" "Jack"
func testBadRange() {
	for _, v := range persons {
		go func() {
			// v is reused at each loop
			fmt.Println(v)
		}()
	}
	time.Sleep(3 * time.Second)
}

func testGoodRange1() {
	for _, v := range persons {
		v := v
		go func() {
			fmt.Println(v)
		}()
	}
	time.Sleep(3 * time.Second)
}

func main() {
	testBreak()

	//	testBadRange()
	testGoodRange1()
}
