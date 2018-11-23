package main

import (
	"fmt"
	"log"
	"runner"
	"time"
)

const (
	// Normal done
	//timeout = 4 * time.Second
	// Will timeout
	timeout = 3 * time.Second
)

func main() {
	log.Println("Start works")

	r := runner.NewRunner(timeout)
	r.Add(createTask(), createTask(), createTask())

	if err := r.Start(); err != nil {
		log.Println("Tasks executing error: ", err)
		return
	}

	log.Println("taks done.")

}

func createTask() func(int) {
	return func(idx int) {
		fmt.Println(idx, " starts working!")
		time.Sleep(time.Duration(idx) * time.Second)
	}
}
