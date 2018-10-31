package main

import (
	"fmt"
	"os"
	"os/signal"
	"syscall"
)

func main() {
	c := make(chan os.Signal, 0)
	// 监听收到的信号
	//signal.Notify(c, syscall.SIGINT)
	// or All
	signal.Notify(c)

	// 取消监听
	//signal.Stop(c)
	for {
		select {
		case s := <-c:
			fmt.Println("Got signal:", s)
			switch s {
			case syscall.SIGINT:
				fmt.Println("Quit!!")
				return
			}
		}
	}
}
