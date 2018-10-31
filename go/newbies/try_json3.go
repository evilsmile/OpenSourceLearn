package main

// To handle 'int' number in json data, which is explained float64 first
import (
	"bytes"
	"encoding/json"
	"fmt"
	"log"
)

func main() {
	records := [][]byte{
		[]byte(`{"status":200, "tag":"one"}`),
		[]byte(`{"status":"ok", "tag":"one"}`),
	}

	for i, record := range records {
		type Record struct {
			StatusCode int64
			StatusName string
			Status     json.RawMessage `json:"status"`
			Tag        string          `json:"tag"`
		}

		var r Record
		if err := json.NewDecoder(bytes.NewReader(record)).Decode(&r); err != nil {
			log.Fatal("json decode failed: ", err)
		}

		var code int64
		err := json.Unmarshal(r.Status, &code)
		if err == nil {
			r.StatusCode = code
		}

		var name string
		err = json.Unmarshal(r.Status, &name)
		if err == nil {
			r.StatusName = name
		}

		fmt.Printf("[%d] %+v\n", i, r)
	}
}
