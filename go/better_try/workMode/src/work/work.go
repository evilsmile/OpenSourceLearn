package work

import (
	"sync"
)

type Work interface {
	Task()
}

type Pool struct {
	work chan Work
	wg   sync.WaitGroup
}

func New(maxPoolWorks int) *Pool {
	p := &Pool{
		work: make(chan Work),
	}

	p.wg.Add(maxPoolWorks)

	for i := 0; i < maxPoolWorks; i++ {
		go func() {
			for w := range p.work {
				w.Task()
			}
			p.wg.Done()
		}()
	}

	return p
}

func (p *Pool) Run(w Work) {
	p.work <- w
}

func (p *Pool) Shutdown() {
	close(p.work)
	p.wg.Wait()
}
