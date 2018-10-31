package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"net/http"
)

func main() {

	// 取消HTTP全局复用连接
	tr := http.Transport{DisableKeepAlives: true}
	client := http.Client{Transport: &tr}

	resp, err := client.Get("http://www.baidu.com")

	if resp != nil {
		defer resp.Body.Close()
	}

	checkError(err)
	defer resp.Body.Close()

	body, err := ioutil.ReadAll(resp.Body)

	checkError(err)

	fmt.Println(string(body))
}

func checkError(err error) {
	if err != nil {
		log.Fatal("error: ", err)
	}
}
