package main

import (
	"fmt"
	"io/ioutil"
	"net/http"
	"os"
	"strconv"
	"sync"
)

var wg sync.WaitGroup

const (
	// nginx export-port
	LOCAL_PROXY_ADDR = "http://127.0.0.1:3344"
)

func test(testPath string) {
	defer wg.Done()

	resp, err := http.Get(LOCAL_PROXY_ADDR + testPath)
	if err != nil {
		fmt.Println("http.Get error: ", err)
		return
	}

	defer resp.Body.Close()

	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		fmt.Println("read body data error: ", err)
		return
	}

	fmt.Println(string(body))
}

func main() {
	testPath := "/abc"
	cnt, _ := strconv.Atoi(os.Args[1])
	for i := 0; i < cnt; i++ {
		wg.Add(1)
		go test(testPath)
	}

	wg.Wait()
}
