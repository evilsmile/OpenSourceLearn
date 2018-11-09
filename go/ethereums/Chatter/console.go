package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

const (
	ENTER_COMMAND_MODE = "Command"
	EXIT_COMMAND_MODE  = "ExitCommand"

	COMMAND_PROMPT_TIP = "@Command"
)

type Mode int

const (
	CommandMode Mode = iota
	ChatMode
)

var currentMode Mode = ChatMode

type Console struct {
	emitter   *Emitter
	commander *Commander
}

func NewConsole(emitter *Emitter, commander *Commander) *Console {
	return &Console{
		emitter:   emitter,
		commander: commander,
	}
}

func (console *Console) Start() {
	promptTip := ""

	handleInput := func(input string) {
		switch input {
		case ENTER_COMMAND_MODE:
			currentMode = CommandMode
			promptTip = COMMAND_PROMPT_TIP
			return
		case EXIT_COMMAND_MODE:
			currentMode = ChatMode
			promptTip = ""
			return
		default:
			switch currentMode {
			case ChatMode:
				select {
				case console.emitter.sendMsgCh <- input:
				}
			case CommandMode:
				select {
				case console.commander.cmdCh <- input:
				}
			}
		}

	}

	go console.emitter.loop()
	go console.commander.loop()

	for {
		fmt.Printf("%s%s: ", console.emitter.self, promptTip)
		inputReader := bufio.NewReader(os.Stdin)
		input, err := inputReader.ReadString('\n')
		if err != nil {
			fmt.Println("read string error ", err)
			continue
		}

		input = strings.TrimSuffix(input, "\n")
		if input == "" {
			continue
		}

		handleInput(input)
	}

}
