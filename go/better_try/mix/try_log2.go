package main

import (
	"io"
	"io/ioutil"
	"log"
	"os"
)

var (
	Error   *log.Logger
	Warning *log.Logger
	Fatal   *log.Logger
	Trace   *log.Logger
)

func init() {
	file, err := os.OpenFile("error.log", os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0666)
	if err != nil {
		log.Fatal("Open error.log failed! ERROR: ", err)
	}
	Warning = log.New(os.Stdout, "WARNING: ", log.Ldate|log.Ltime|log.Lshortfile)
	Fatal = log.New(os.Stdout, "FATAL: ", log.Ldate|log.Ltime|log.Lshortfile)
	// ioutil.Discard 中实现了io.Writer接口，它不会有动作，但是成功会返回
	Trace = log.New(ioutil.Discard, "Trace: ", log.Ldate|log.Ltime|log.Lshortfile)
	// io.MultiWriter 是一个变参函数，可以接受实现任意个实现了io.Writer接口的值，
	// 它会返回一个io.Writer，当向这个返回值写数据时，会向所有绑定在一起的io.Writer写入
	Error = log.New(io.MultiWriter(file, os.Stderr), "ERROR: ", log.Ldate|log.Ltime|log.Lshortfile)
}

func main() {
	Trace.Println("trace message")
	Fatal.Println("fatal message")
	Warning.Println("warning message")
	Error.Println("error message")
}
