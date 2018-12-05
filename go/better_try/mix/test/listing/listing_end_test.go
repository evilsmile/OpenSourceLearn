package listing

import (
	"encoding/json"
	"handler"
	"net/http"
	"net/http/httptest"
	"testing"
)

const (
	checkMark = "\u2713" // 这是个勾
	bollX     = "\u2717" // 这是个叉
)

func init() {
	handler.Routes()
}

func TestSendJson(t *testing.T) {
	t.Log("Test SendJSON endpoint")
	{
		req, err := http.NewRequest("GET", "/sendjson", nil)
		if err != nil {
			t.Fatal("\t\tShould create a new request. ", bollX, err)
		}
		t.Log("\t\tShould create a new request. ", checkMark)

		rw := httptest.NewRecorder()
		// 服务的默认多路选择器(Mux) 。调用这个方法模仿了外部客户端对 /sendjson 服务端点的请求。
		// 一旦 ServeHTTP 方法调用完成，http.ResponseRecorder 值就包含了 SendJSON 处理函数的响应。
		http.DefaultServeMux.ServeHTTP(rw, req)

		if rw.Code != 200 {
			t.Fatalf("\t\tShould receive \"200\", but get %d. %s\n", rw.Code, bollX)
		}
		t.Log("\t\tShould receive 200.", checkMark)

		u := struct {
			Name  string
			Email string
		}{}

		if err := json.NewDecoder(rw.Body).Decode(&u); err != nil {
			t.Fatal("\t\tShould decode json reply.", bollX)
		}
		t.Log("\t\tShould decode json reply.", checkMark)

		if u.Name != "Bill" {
			t.Fatal("\t\tShould have name 'Bill'", bollX)
		} else {
			t.Log("\t\tShould have name 'Bill'", checkMark)
		}

		if u.Email != "bill@software.cm" {
			t.Fatal("\t\tShould have email 'bill@software.com'", bollX)
		} else {
			t.Log("\t\tShould have email 'bill@software.com'", checkMark)
		}
	}
}
