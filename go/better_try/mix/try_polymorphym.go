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

func (u User) Notify() {
	fmt.Println("User ", u.Name, " notifying!!")
}

type Admin struct {
	Name string
}

func (a *Admin) Notify() {
	fmt.Println("Admin ", a.Name, " notifying!!")
}

func sendNotify(n Notifier) {
	n.Notify()
}

func main() {

	u := User{Name: "User1"}
	// 如果User的Notify是以值作为接收者，则既可以用指针也可以用值
	sendNotify(u)
	sendNotify(&u)

	a := Admin{Name: "Admin1"}
	// 但是Admin的Notify是以指针作为接收者，则只能传入指针，因为可能获取不到值的指针，
	// 如Duration(64).Notify()，Duration(64)这个值获取不到指针
	//	sendNotify(a)
	sendNotify(&a)
}
