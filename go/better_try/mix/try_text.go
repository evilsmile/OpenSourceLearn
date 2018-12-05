package main

import (
	"fmt"
	"os"
	"text/template"
)

type HostData struct {
	Hostname string
	Domain   string
}

func main() {
	// .Hostname means field of Hostname of one struct
	//temp := "{{.Hostname}}.{{.Domain}}\n"
	// printf is a function
	//temp := "{{printf \"domain is %s.%s -->\" .Hostname .Domain}}\n"
	temp := "{{ printf \"domain is %s -->\" (print .Hostname .Domain)}}\n"

	t, err := template.New("ppp").Parse(temp)
	if err != nil {
		fmt.Println("Create template parser error: ", err)
		return
	}

	data := HostData{"evil", "example.com"}
	err = t.Execute(os.Stdout, data)
	if err != nil {
		fmt.Println("Parse text tempalte err: ", err)
		return
	}
}
