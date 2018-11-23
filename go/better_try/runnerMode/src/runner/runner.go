package runner

import (
	"errors"
	"os"
	"os/signal"
	"time"
)

type Runner struct {
	interrupt chan os.Signal
	complete  chan error
	timeout   <-chan time.Time
	tasks     []func(int)
}

var ErrTimeout = errors.New("Error timeout!")
var ErrInterrupt = errors.New("Error interrupted!")

func NewRunner(d time.Duration) *Runner {
	return &Runner{
		// 使用带缓冲的
		interrupt: make(chan os.Signal, 1),
		complete:  make(chan error),
		timeout:   time.After(d),
	}
}

func (r *Runner) Add(f ...func(int)) {
	r.tasks = append(r.tasks, f...)
}

func (r *Runner) Start() error {

	signal.Notify(r.interrupt, os.Interrupt)
	go func() {
		r.complete <- r.run()
	}()

	select {
	case err := <-r.complete:
		return err
	case <-r.timeout:
		return ErrTimeout
	}
}

func (r *Runner) run() error {
	for id, runner := range r.tasks {
		if r.gotInterrupt() {
			return ErrInterrupt
		}
		runner(id)
	}
	return nil
}

func (r *Runner) gotInterrupt() bool {

	select {
	case <-r.interrupt:
		signal.Stop(r.interrupt)
		return true
	default:
		return false
	}
}
