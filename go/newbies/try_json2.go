package main

// To handle 'int' number in json data, which is explained float64 first
import (
	"bytes"
	"encoding/json"
	"fmt"
	"log"
)

var data = []byte(`{"status":200}`)

func decodeInt1() {
	var result map[string]interface{}

	if err := json.Unmarshal(data, &result); err != nil {
		log.Fatal("Error unmarshal: ", err)
	}

	fmt.Printf("%T\n", result["status"])
	var status = int64(result["status"].(float64))
	fmt.Println("status: ", status)

}

func decodeInt2() {
	var result map[string]interface{}

	decoder := json.NewDecoder(bytes.NewReader(data))
	decoder.UseNumber()

	if err := decoder.Decode(&result); err != nil {
		log.Fatal("Error unmarshal: ", err)
	}

	fmt.Printf("%T\n", result["status"])
	status, _ := result["status"].(json.Number).Int64()
	fmt.Println("status: ", status)
}

func decodeInt3() {
	type Data struct {
		Status uint64 `json:"status"`
	}

	var d Data
	if err := json.NewDecoder(bytes.NewReader(data)).Decode(&d); err != nil {
		log.Fatal("Error unmarshal: ", err)
	}

	fmt.Println("status: ", d.Status)
}

func main() {
	//decodeInt1()
	//decodeInt2()
	decodeInt3()
}
