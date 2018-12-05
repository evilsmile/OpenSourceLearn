package listing

import (
	_ "encoding/xml"
	"fmt"
	"net/http"
	"net/http/httptest"
	"testing"
)

const (
	checkMark = "\u2713" // 这是个勾
	bollX     = "\u2717" // 这是个叉
)

var feed = `<?xml version="1.0" encoding="utf-8">
<rss>
<channel>
  <title>Go golang program</title>
  <description>Go: https://github.com/golang</description>
  <link>http://gogoing.net</link>
  <item>
    <pubDate>Sun, 15 Mar 2015 15:40:23</pubDate>
	<description>Go is an object oriented language</description>
  </item>
</channel>
</rss>`

func mockServer() *httptest.Server {
	f := func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(200)
		w.Header().Set("Content-Type", "application/xml")
		fmt.Fprintln(w, feed)
	}
	return httptest.NewServer(http.HandlerFunc(f))
}

func TestDownload(t *testing.T) {

	server := mockServer()
	defer server.Close()

	var urls = []struct {
		url        string
		statusCode int
	}{
		{
			server.URL,
			http.StatusOK,
		},
	}

	t.Log("Given the need to test downloading content")
	{

		for _, u := range urls {
			url := u.url
			statusCode := u.statusCode
			t.Logf("Checking the download of '%s' for status '%d'", url, statusCode)
			{
				rsp, err := http.Get(url)
				if err != nil {
					t.Fatalf("\t\tShould be able to make the Get call. %s \n\t\tReason:[%s]\n", bollX, err)
				}
				t.Log("Should be able to make Get call.", checkMark)

				defer rsp.Body.Close()

				if rsp.StatusCode == statusCode {
					t.Logf("Should receive statusCode '%d'. %v", rsp.StatusCode, checkMark)
				} else {
					t.Errorf("Should receive a statusCode of '%d', but get %v %v", statusCode, bollX, rsp.StatusCode)
				}
			}
		}
	}

}
