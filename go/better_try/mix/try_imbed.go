package main

import (
	"fmt"
)

type Notifier interface {
	Notify()
}

type User struct {
	Name string
}

func (u *User) Notify() {
	fmt.Println(u.Name, " is notifying...")
}

type Admin struct {
	User
	level string
}

func main() {
	a := Admin{
		User: User{
			Name: "Amin1",
		},
		level: "super",
	}
	sendNotify(&a)
}

func sendNotify(n Notifier) {
	n.Notify()
}
