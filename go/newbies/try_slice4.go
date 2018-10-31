package main

import (
	"fmt"
)

func main() {

	s1 := []int{1, 2, 3}
	// s2 指向s1的底层数据， 修改s1就修改了s2
	s2 := s1[1:]
	s1[2] += 10
	fmt.Printf("s1: %d %d %v %v\n", len(s1), cap(s1), &s1[0], s1)
	fmt.Printf("s2: %d %d %v %v\n", len(s2), cap(s2), &s2[0], s2)

	// s1 重新分配了内容，修改s1不会修改s2
	s1 = append(s1, 4)
	s1[2] += 20
	fmt.Printf("s1: %d %d %v %v\n", len(s1), cap(s1), &s1[0], s1)
	fmt.Printf("s2: %d %d %v %v\n", len(s2), cap(s2), &s2[0], s2)

}
