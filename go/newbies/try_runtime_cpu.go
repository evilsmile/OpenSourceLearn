package main

import (
	"fmt"
	"runtime"
)

func main() {
	// Get current GOMAXPROCS
	fmt.Println(runtime.GOMAXPROCS(-1))
	fmt.Println(runtime.NumCPU())

	done := false

	go func() {
		done = true
	}()

	// 你的程序可能出现一个 goroutine 在运行时阻止了其他 goroutine 的运行，比如程序中有一个不让调度器运行的 for 循环
	for !done {
		// Gosched() 手动启动调度器
		runtime.Gosched()
		fmt.Println("NOt done!")
	}
	fmt.Println("done!")

}
