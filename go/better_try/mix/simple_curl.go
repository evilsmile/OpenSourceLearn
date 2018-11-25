package main

import (
	"io"
	"log"
	"net/http"
	"os"
)

func main() {

	r, err := http.Get(os.Args[1])
	if err != nil {
		log.Fatal("http Get error: ", err)
	}

	f, err := os.Create(os.Args[2])
	if err != nil {
		log.Fatal("create file error: ", err)
	}
	defer f.Close()

	dst := io.MultiWriter(os.Stdout, f)
	io.Copy(dst, r.Body)

	if err = r.Body.Close(); err != nil {
		log.Fatal("close response body failed: ", err)
	}

}
