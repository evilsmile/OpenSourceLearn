package main

import (
	"fmt"
	"os"
	"path/filepath"
	"time"
)

func main() {
	var i int = 1
	// defer在定义的时候就已经把值算好了，而不是在执行时
	defer fmt.Println("Result is ", func() int { return i * 2 }())
	i += 10

	if len(os.Args) != 2 {
		fmt.Println("Usage: dir")
		return
	}

	dir := os.Args[1]
	stat, err := os.Stat(dir)
	if err != nil || !stat.IsDir() {
		fmt.Println("Err file state: ", dir, " err: ", err)
		return
	}

	var targets []string
	filepath.Walk(dir, func(fPath string, fInfo os.FileInfo, err error) error {
		if err != nil {
			return err
		}

		if !fInfo.Mode().IsRegular() {
			return nil
		}

		targets = append(targets, fPath)
		return nil
	})

	for _, file := range targets {
		// defer的执行时机是在调用函数返回，而不是作用域， 所以 为了避免打开文件过多没有及时关闭，
		// 需要放到匿名函数中
		func() {
			f, err := os.Open(file)
			if err != nil {
				fmt.Println("err open file ", file, ". err: ", err)
				return
			}
			defer f.Close()
		}()
	}

	time.Sleep(10 * time.Second)
}
