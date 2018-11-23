package main

// 示例中启动了3个监控goroutine进行不断的监控，每一个都使用了Context进行跟踪，当我们使用cancel函数通知取消时，
// 这3个goroutine都会被结束。这就是Context的控制能力，它就像一个控制器一样，按下开关后，所有基于这个Context
// 或者衍生的子Context都会收到通知，这时就可以进行清理操作了，最终释放goroutine，这就优雅的解决了goroutine
// 启动后不可控的问题。

// 不要把Context放在结构体中，要以参数的方式传递
// 以Context作为参数的函数方法，应该把Context作为第一个参数，放在第一位。
// 给一个函数方法传递Context的时候，不要传递nil，如果不知道传递什么，就使用context.TODO
// Context的Value相关方法应该传递必须的数据，不要什么数据都使用这个传递
// Context是线程安全的，可以放心的在多个goroutine中传递

import (
	"context"
	"fmt"
	"time"
)

func watch(ctx context.Context, name string) {
	fmt.Println("begin monitoring ", name)
	for {
		select {
		case <-ctx.Done():
			fmt.Println(name, " watch done.")
			return
		default:
		}

		var ctxValue string
		if ctx.Value("key") != nil {
			ctxValue = ctx.Value("key").(string)
		}
		fmt.Println(name, ctxValue, " monitoring....")
		time.Sleep(2 * time.Second)
	}
}

// type Context interface {
//	Deadline() (deadline time.Time, ok bool)
// 	Done() <-chan struct{}
// 	Err() error
// 	Value(key interface{}) interface{}
// }

func main() {

	ctx, cancel := context.WithCancel(context.Background())
	subCtx1 := context.WithValue(ctx, "key", "MONITOR1")

	go watch(subCtx1, "[M1]")
	go watch(ctx, "[M2]")
	go watch(ctx, "[M3]")

	time.Sleep(10 * time.Second)
	fmt.Println("Cancel all monitor.")
	cancel()
	fmt.Println("Waiting for all monitor stopped.")
	time.Sleep(5 * time.Second)
}
