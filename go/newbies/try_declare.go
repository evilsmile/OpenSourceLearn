package main

import (
	"sync"
)

// 定义自己的类型
type myMutex sync.Mutex

func testMutex1() {
	//var mutex myMutex
	// 非interface{}类型创建新类型时，不会继承原有的方法
	//mutex.Lock()
}

type myMutex2 struct {
	// 可以通过把原类型通过匿名字段的形式加到新结构体中
	sync.Mutex
}

func testMutex2() {
	var mutex myMutex2
	mutex.Lock()
	mutex.Unlock()
}

type myLock sync.Locker

// interface 可以保留方法集
func testLock() {
	var lock myLock
	lock.Lock()
	lock.Unlock()
}

func main() {
	testMutex1()
	testMutex2()
	testLock()

}
