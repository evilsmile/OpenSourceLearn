use std::thread;
use std::sync::mpsc;
use std::sync::Arc;
use std::sync::Mutex;

enum Message {
    NewJob(Job),
    Terminate,
}

pub struct ThreadPool {
    workers: Vec<Worker>,
    sender: mpsc::Sender<Message>,
}


trait FnBox {
    // Take 'self: Box<Self> to take ownership of 'self' and move the 
    // value out of the 'Box<T>'.
    fn call_box(self: Box<Self>);
}

// impl 'FnBox' trait for any type 'F' that implements the 'FnOnce()'.
// This means that any 'FnOnce()' closure use our 'call_box' method.
impl<F: FnOnce()> FnBox for F {
    fn call_box(self: Box<F>) {
        // move the closure out of 'Box<T>' and call the closure.
        (*self)()
    }
}

type Job = Box<dyn FnBox + Send + 'static>;

impl ThreadPool {
    /// Create a new thread pool.
    /// The size is the number of threads in the pool.
    /// # Panics
    ///
    /// The 'new' function will panic if the size is zero.
    pub fn new(size: usize) -> ThreadPool {
        assert!(size > 0);

        // create new channel and have the pool hold the sending end
        let (sender, receiver) = mpsc::channel();

        let receiver = Arc::new(Mutex::new(receiver));

        // 'with_capacity' preallocates space in the vector.
        let mut workers = Vec::with_capacity(size);

        for id in 0..size {
            // Share 'receiver' between threads.
            // Arc will bump the reference
            workers.push(Worker::new(id, Arc::clone(&receiver)));
        }

        ThreadPool {
            workers,
            sender,
        }
    }

    pub fn execute<F>(&self, f: F) 
        where F: FnOnce() + Send + 'static    // 'FnOnce' means the thread running a request will only be executed one time, 
                                              //     '()' represent a closure that takes no parameter and no return value;
                                              // 'Send' to transfer the closure from one thread to another;
                                              // 'static' because we don't know how long the thread will exeucte.
    {
        let job = Box::new(f);
        self.sender.send(Message::NewJob(job)).unwrap();
    }
}

impl Drop for ThreadPool {
    fn drop(&mut self) {
        println!("Sending terminate message to all workers.");

        for _ in &mut self.workers {
            self.sender.send(Message::Terminate).unwrap();
        }

        println!("Shutting down all workers.");

        for worker in &mut self.workers {
            println!("Shutting down worker {}", worker.id);

            if let Some(thread) = worker.thread.take() {
                thread.join().unwrap();
            }
        }
    }
}

struct Worker {
    id: usize,
    thread: Option<thread::JoinHandle<()>>,
}

impl Worker {
    fn new(id: usize, receiver: Arc<Mutex<mpsc::Receiver<Message>>>) -> Worker {
        let thread = thread::spawn(move || {
            loop {
                let message = receiver.lock().unwrap().recv().unwrap();

                match message {
                    Message::NewJob(job) => {

                        println!("Worker {} got a job; executing.", id);

                        job.call_box();
                    },
                    Message::Terminate => {
                        println!("Worker {} was told to terminate", id);
                        break;
                    },
                }
            }
        });

        Worker {
            id,
            thread: Some(thread),
        }
    }
}
