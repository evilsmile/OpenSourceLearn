package main

import (
	"fmt"
	"io"
	"pool"
	"sync"
	"sync/atomic"
	"time"
)

const (
	maxPoolSize = 2
	queryNum    = 10
)

var (
	wg sync.WaitGroup
)

type DBConnection struct {
	ID int32
}

func (dbCon *DBConnection) Close() error {
	fmt.Printf("Db connection [%d] closed.\n", dbCon.ID)
	return nil
}

var connId int32

func CreateNewConn() (io.Closer, error) {
	id := atomic.AddInt32(&connId, 1)
	return &DBConnection{
		ID: id,
	}, nil
}

func main() {

	wg.Add(queryNum)

	p := pool.New(CreateNewConn, maxPoolSize)

	for i := 0; i < queryNum; i++ {
		// 这里使用了i的副本作查询
		go func(idx int) {
			query(idx, p)
			wg.Done()
		}(i)
	}

	wg.Wait()
	p.Close()

}

func query(idx int, p *pool.Pool) {

	r, err := p.Acquire()
	if err != nil {
		fmt.Printf("query[%d] error:'%s'\n", idx, err)
		return
	}
	defer p.Release(r)

	time.Sleep(200 * time.Millisecond)
	fmt.Printf("Querying [%d] with connId [%d]\n", idx, r.(*DBConnection).ID)
}
