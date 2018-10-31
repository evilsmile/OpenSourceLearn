package main

import "fmt"

func f2(c chan string) {
	for {
		n := <-c
		fmt.Println("f2 say meet ", n)
	}
}

func f1(c chan string) {
	for {
		n := <-c
		fmt.Println("f1 say meet ", n)
	}
}

/*
func subroutine(commu chan string) {

	close(commu)
	commu <- "ssss"

}
*/

func main() {

	c := make(chan string)
	go f2(c)
	go f1(c)

	for i := 0; i < 100; i++ {
		c <- fmt.Sprintf("name%d", i)
	}

	c2 := make(chan struct{}, 10)
	for i := 0; i < 10; i++ {
		c2 <- struct{}{}
	}

	i := 0
	for {
		<-c2
		fmt.Println("Get a c2:", i)
		i++

		if i > 9 {
			break
		}
	}

	/*
		commu := make(chan string)

		go subroutine(commu)

		msg := <-commu

		fmt.Println("msg: ", msg)
	*/

}
