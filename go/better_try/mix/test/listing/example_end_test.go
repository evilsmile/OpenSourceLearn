package listing

import (
	"encoding/json"
	"fmt"
	"net/http"
	"net/http/httptest"
)

const (
	checkMark = "\u2713" // 这是个勾
	bollX     = "\u2717" // 这是个叉
)

func ExampleSendJson() {
	req, _ := http.NewRequest("GET", "/sendjson", nil)
	rw := httptest.NewRecorder()
	// 服务的默认多路选择器(Mux) 。调用这个方法模仿了外部客户端对 /sendjson 服务端点的请求。
	// 一旦 ServeHTTP 方法调用完成，http.ResponseRecorder 值就包含了 SendJSON 处理函数的响应。
	http.DefaultServeMux.ServeHTTP(rw, req)

	u := struct {
		Name  string
		Email string
	}{}

	if err := json.NewDecoder(rw.Body).Decode(&u); err != nil {
		return
	}

	fmt.Println(u.Email)
	//Output:
	//{bill@software.com}
}
