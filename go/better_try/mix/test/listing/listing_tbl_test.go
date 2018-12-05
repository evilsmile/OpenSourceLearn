package listing

import (
	"net/http"
	"testing"
)

const (
	checkMark = "\u2713" // 这是个勾
	bollX     = "\u2717" // 这是个叉
)

func TestDownload(t *testing.T) {

	var urls = []struct {
		url        string
		statusCode int
	}{
		{
			"http://www.goinggo.net/feeds/posts/default?alt=rss",
			http.StatusOK,
		},
		{
			"http://rss.cnn.com/rss/cnn_topstbadurl.rss",
			http.StatusNotFound,
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
