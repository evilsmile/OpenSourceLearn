package main

import (
	"flag"
	"fmt"
	"log"
	"net/http"
	"os"
)

type Count int

func (cnt *Count) ServeHTTP(w http.ResponseWriter, req *http.Request) {
	*cnt++
	fmt.Fprintf(w, "counter = %d", *cnt)
}

func ArgServer(w http.ResponseWriter, req *http.Request) {

	fmt.Fprintln(w, os.Args)
}

var addr = flag.String("addr", ":1718", "http service address") // Q=17, R=18

func main() {

	ctr := new(Count)
	http.Handle("/count", ctr)
	http.Handle("/args", http.HandlerFunc(ArgServer))

	err := http.ListenAndServe(*addr, nil)
	if err != nil {
		log.Fatal("ListenAndServe:", err)
	}
}
