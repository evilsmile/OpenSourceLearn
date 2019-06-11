use std::io::prelude::*;
use std::net::TcpStream;
use std::net::TcpListener;
use std::fs;
use std::thread;
use std::time::Duration;

use http_server::ThreadPool;

fn main() {
    // 'bind' returns a new 'TcpListener' instance (returns a 'Result<T, E>')
    let listener = TcpListener::bind("0.0.0.0:7878").unwrap();

    // create a new thread pool with a configurable number of threads.
    let pool = ThreadPool::new(4);

    // 'incoming' returns an iterator that gives us a sequence of streams
    // (more specifically, streams of type 'TcpStream').
    // A single stream represents an open connection between C/S.
    for stream in listener.incoming().take(2) {
        let stream = stream.unwrap();
        println!("Connection established!");

        // create a new thread and then run the code in the closure in the 
        // new thread
        pool.execute(|| {
            handle_conn(stream);
        });
    }
    
    println!("Shutting down.");
}

// use 'mut', cause 'TcpStream' instance keeps track of what data it returns 
// to us internally. It might read more data we asked for and save that data
// for the next time we ask for data.
fn handle_conn(mut stream: TcpStream) {
    // 512 bytes
    let mut buffer = [0; 512];

    // Read bytes from 'TcpStream' and put them in the buffer.
    stream.read(&mut buffer).unwrap();

    // convert bytes to a string. 'from_utf8_lossy' takes a '&[u8]' => 'String'
    // 'lossy' means when it sees an invalid UTF-8 sequence: it will replace
    // the invalid sequence with '�', the 'U+FFFD REPLACEMENT CHARACTER'
    //println!("Request: {}", String::from_utf8_lossy(&buffer[..]));

    // byte string
    let get = b"GET / HTTP/1.1\r\n";
    let sleep = b"GET /sleep HTTP/1.1\r\n";

    let (status_line, filename) = if buffer.starts_with(get) {
        ("HTTP/1.1 200 OK\r\n\r\n", "hello.html")
    } else if buffer.starts_with(sleep) {
        println!("sleep for a while");
        thread::sleep(Duration::from_secs(5));
        ("HTTP/1.1 200 OK\r\n\r\n", "hello.html")
    } else {
        ("HTTP/1.1 404 NOT FOUND\r\n\r\n", "404.html")
    };

    let contents = fs::read_to_string(filename).unwrap();
    let response = format!("{}{}", status_line, contents);
    stream.write(response.as_bytes()).unwrap();
    stream.flush().unwrap(); 
}
