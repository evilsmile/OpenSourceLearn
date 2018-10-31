/*
 Just for free try of golang.
*/
package main

import (
	"fmt"
)

var a = 234

// 简短声明只能在函数内使用
// a := 234

func test() {
	// below will get panic
	//var mAgeInfo map[string]int
	// mAgeInfo["ll"] = 234

	mAgeInfo := make(map[string]int, 99)
	mAgeInfo["ll"] = 234
	// map can't be 'cap'ed
	//cap(mAgeInfo)
	fmt.Println("age:", mAgeInfo["ll"])

	// use pointer to change array
	x := [3]int{1, 2, 3}
	func(newX *[3]int) {
		(*newX)[1] = 888
	}(&x)
	fmt.Printf("x[1]:%d\n", x[1])

	// use slice to change array
	y := []int{1, 2, 3}
	func(newY []int) {
		newY[1] = 888
	}(y)
	fmt.Printf("y[1]:%d\n", y[1])

}

func main() {
	test()

	fmt.Println("a:", a)
	b := "Hey world!"
	fmt.Println(b)
}
