package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"net/http"
)

func main() {

	resp, err := http.Get("http://www.baidu.comj")

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
