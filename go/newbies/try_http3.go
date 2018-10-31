package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"net/http"
)

func main() {
	req, err := http.NewRequest("GET", "http://www.baidu.com", nil)
	checkError(err)

	req.Close = true
	//req.Header.Add("Connection", "close")

	resp, err := http.DefaultClient.Do(req)
	if resp != nil {
		defer resp.Body.Close()
	}
	checkError(err)

	body, err := ioutil.ReadAll(resp.Body)
	checkError(err)

	fmt.Println(string(body))
}

func checkError(err error) {
	if err != nil {
		log.Fatal("error: ", err)
	}
}
