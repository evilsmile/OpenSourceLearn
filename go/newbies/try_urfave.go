package main

import (
	"fmt"
	"gopkg.in/urfave/cli.v1"
	"log"
	"os"
	"sort"
)

func main() {
	var language string

	app := cli.NewApp()
	app.Name = "GoTest"
	app.Usage = "Hello world"
	app.Version = "1.2.3"
	app.Action = func(c *cli.Context) error {
		fmt.Println("Boom!")
		fmt.Println(c.String("lang"), c.Int("port"))
		fmt.Println(language)
		return nil
	}

	app.Flags = []cli.Flag{
		cli.IntFlag{
			Name:  "port, p",
			Value: 8000,
			Usage: "listening port",
		},
		cli.StringFlag{
			Name:        "lang, l",
			Value:       "english",
			Usage:       "read from `FILE`",
			Destination: &language,
		},
	}

	cli.HelpFlag = cli.BoolFlag{
		Name:  "help, h",
		Usage: "Display this message",
	}
	cli.VersionFlag = cli.BoolFlag{
		Name:  "print-version, v",
		Usage: "print version",
	}

	sort.Sort(cli.FlagsByName(app.Flags))

	app.Commands = []cli.Command{
		{
			Name:     "add",
			Aliases:  []string{"a"},
			Usage:    "calc 1+1",
			Category: "arithmetic",
			Action: func(c *cli.Context) error {
				fmt.Println("1+1=", 1+1)
				return nil
			},
		},
	}

	app.Before = func(c *cli.Context) error {
		fmt.Println("app Before")
		return nil
	}
	app.After = func(c *cli.Context) error {
		fmt.Println("app After")
		return nil
	}

	err := app.Run(os.Args)
	if err != nil {
		log.Fatal(err)
	}

}
