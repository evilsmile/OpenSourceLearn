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

	url := "http://www.goinggo.net/feeds/posts/default?alt=rss"
	statusCode := 200

	t.Log("Given the need to test downloading content")
	{

		t.Logf("Checking the download of '%s' for status '%d'", url, statusCode)
		{
			rsp, err := http.Get(url)
			if err != nil {
				t.Fatal("\t\tShould be able to make the Get call.", bollX, err)
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
