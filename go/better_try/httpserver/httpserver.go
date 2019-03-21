package main

import (
	"fmt"
	"gopkg.in/urfave/cli.v1"
	"net/http"
	"os"
	"path/filepath"
	"sync"
)

var (
	startCommand = cli.Command{
		Name:      "start",
		Action:    handler,
		Usage:     "start server",
		ArgsUsage: "",
		Flags: []cli.Flag{
			PortFlag,
		},
		Description: "Desc",
	}

	PortFlag = cli.StringFlag{
		Name:  "port",
		Usage: "server listen port",
	}
)

func IndexHandler(w http.ResponseWriter, r *http.Request) {
	fmt.Fprintln(w, "hello world")
}

func abcHandler(w http.ResponseWriter, r *http.Request) {
	fmt.Fprintln(w, "h")
}

func initOnce() {
	fmt.Println("Initialized only once")
}

// 如果某个handler依赖外部数据, 通过传参
func wrapperHandler(tips string) http.HandlerFunc {
	var doOnce sync.Once

	return func(w http.ResponseWriter, r *http.Request) {
		doOnce.Do(initOnce)
		fmt.Println(tips, " called!")
		switch tips {
		case "Index":
			IndexHandler(w, r)
		case "abc":
			abcHandler(w, r)
		}
	}
}

func handler(ctx *cli.Context) error {
	if args := ctx.Args(); len(args) > 0 {
		return fmt.Errorf("invalid-command: %q", args[0])
	}

	port := ctx.String(PortFlag.Name)
	fmt.Println("Port: ", port)

	http.HandleFunc("/", wrapperHandler("Index"))
	http.HandleFunc("/abc", wrapperHandler("abc"))
	http.ListenAndServe("127.0.0.1:"+port, nil)

	return nil
}

func main() {

	app := cli.NewApp()
	app.Name = filepath.Base(os.Args[0])
	app.Author = ""
	app.Version = "0.0.1"
	app.Usage = "Go http server"
	//	app.Action = handler
	app.Flags = []cli.Flag{}
	app.Commands = []cli.Command{
		startCommand,
	}

	if err := app.Run(os.Args); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(-1)
	}

}
