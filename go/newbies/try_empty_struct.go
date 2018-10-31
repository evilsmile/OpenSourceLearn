package main

import "fmt"
import "unsafe"

type S struct {
	i int
	s string
}

func (s *S) addr() {
	fmt.Printf("%p\n", s)
}

func main() {
	var s string
	var ary [3]int
	var ss S
	var es struct{}
	var esp = &es
	var ess [100]struct{}
	var x = make([]struct{}, 100)
	var x1 = x[:50]

	fmt.Println("string size: ", unsafe.Sizeof(s))
	fmt.Println("array size: ", unsafe.Sizeof(ary))
	fmt.Println("S size: ", unsafe.Sizeof(ss))
	ss.addr()
	fmt.Println("es size: ", unsafe.Sizeof(es), " &es: ", esp)
	fmt.Println("ess size: ", unsafe.Sizeof(ess))
	fmt.Println("x size: ", unsafe.Sizeof(x))
	fmt.Println("x1 len: ", len(x1), ", cap:", cap(x1))

}
