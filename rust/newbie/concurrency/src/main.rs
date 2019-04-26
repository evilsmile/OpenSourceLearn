use std::thread;
use std::sync::mpsc;   // multiple producer, single consumer
use std::time::Duration;

use std::sync::{Mutex, Arc};

fn test_mutex() {
    let m = Mutex::new(5);
    {
        let mut num = m.lock().unwrap();
        *num = 6;
    }
    println!("m = {:?}", m);

    // --- multiple ---
    let counter = Arc::new(Mutex::new(0));
    let mut handlers = vec![];

    for _ in 0..10 {
        let counter = Arc::clone(&counter);
        let h = thread::spawn(move || {
            let mut c = counter.lock().unwrap();
            *c += 1;
        });
        handlers.push(h);
    }

    for h in handlers {
        h.join().unwrap();
    }

    println!("counter is {}", counter.lock().unwrap());
}

fn main() {

    let v = vec![1,2,3];

    let (tx, rx) = mpsc::channel();

    let tx2 = mpsc::Sender::clone(&tx);
    let tx3 = mpsc::Sender::clone(&tx);

    // 用move把v的所有权转移了,不然rust不知道v在线程里运行多久,导致编译错误
    let handle = thread::spawn(move || {
        println!("Here is a vector: {:?}", v);

        let vals = vec![
            String::from("hi"),
            String::from("hey"),
            String::from("hello"),
        ];

        for val in vals {
            tx.send(val).unwrap();
            thread::sleep(Duration::from_millis(1));
        }
    });

    let handle2 = thread::spawn(move || {
        let vals = vec![
            String::from("more"),
            String::from("message"),
            String::from("for"),
            String::from("you"),
        ];
        for val in vals {
            tx2.send(val).unwrap();
            thread::sleep(Duration::from_millis(1));
        }
    });

    let handle3 = thread::spawn(move || {
        let vals = vec![
            String::from("more3"),
            String::from("message3"),
            String::from("for3"),
            String::from("you3"),
        ];
        for val in vals {
            tx3.send(val).unwrap();
            thread::sleep(Duration::from_millis(1));
        }
    });

    // 在这里join的话 main 会等 thread结束后才往下执行
    // handle.join().unwrap();

    //let received = rx.recv().unwrap();

    for received in rx {
        println!("Got {} from thread", received);
    }

    test_mutex();

    handle.join().unwrap();
    handle2.join().unwrap();
    handle3.join().unwrap();
}
