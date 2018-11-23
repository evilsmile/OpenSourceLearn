package pool

import (
	"errors"
	"io"
	"log"
	"sync"
)

var (
	ErrClosed = errors.New("Pool Closed")
)

type Pool struct {
	resource chan io.Closer
	factory  func() (io.Closer, error)
	mutex    sync.Mutex
	close    bool
}

func New(f func() (io.Closer, error), size int) *Pool {
	return &Pool{
		resource: make(chan io.Closer, size),
		factory:  f,
	}
}

func (p *Pool) Acquire() (io.Closer, error) {

	select {
	case r, ok := <-p.resource:
		if !ok {
			return nil, ErrClosed
		}
		return r, nil
	default:
		return p.factory()

	}
}

func (p *Pool) Release(r io.Closer) {
	p.mutex.Lock()
	defer p.mutex.Unlock()

	if p.close {
		r.Close()
		return
	}

	select {
	case p.resource <- r:
		log.Println("Release:", "In Queue")
	default:
		r.Close()

	}

}

func (p *Pool) Close() {
	p.mutex.Lock()
	defer p.mutex.Unlock()

	if p.close {
		return
	}

	p.close = true

	close(p.resource)

	for r := range p.resource {
		r.Close()
	}
}
