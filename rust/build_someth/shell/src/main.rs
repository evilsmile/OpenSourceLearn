use std::io;
use std::process;
use std::path::Path;
use std::env;
use std::io::Write;

fn main() {

    loop {
        // use the `>` character as the prompt
        // need to explicitly flush this to ensure it prints before read_line
        print!(">");
        // need import 'Write' trait cause flush is its fn
        io::stdout().flush();

        let mut input = String::new();
        io::stdin().read_line(&mut input).unwrap();

        let mut commands = input.trim().split(" | ").peekable();
        let mut previous_command = None;

        while let Some(command) = commands.next() {

            let mut parts = command.trim().split_whitespace();
            let command = parts.next().unwrap();
            let args = parts;

            match command {
                // built-in commands 
                "exit" => return,
                "cd" => {
                    // 'peekable' creates an iterator which can use 'peek' to look at the next element
                    // of the iterator without consuming it.
                    // 'peek' returns a reference to the next() value without advancing the iterator.
                    // 'map_or' returns a default "/" or "*x".
                    let new_dir = args.peekable().peek().map_or("/", |x| *x);
                    // 'Path' supports a number of operations for inspecting a path.
                    let root = Path::new(new_dir);
                    // change the current working directory to the specified path
                    if let Err(e) = env::set_current_dir(&root) {
                        eprintln!("{}", e);
                    }

                    previous_command = None;
                },
                command => {

                    let stdin = previous_command.map_or(
                            process::Stdio::inherit(),
                            |output: process::Child| process::Stdio::from(output.stdout.unwrap())
                        );

                    let stdout = if commands.peek().is_some() {
                        // there is another command piped behind this one.
                        // prepare to send output to the next command.
                        // 'piped' create a new pipe to connect the parent and child process.
                        process::Stdio::piped()
                    } else {
                        // no more commands piped behind,
                        // send output to shell stdout.
                        // 'inherit' make child inherit from the corresponding parent descriptor.
                        process::Stdio::inherit()
                    };

                    // 'Commander' is a process builder, providing find-grained control over how a new process
                    // should be spawned.
                    let output = process::Command::new(command)
                        .args(args)
                        .stdin(stdin)
                        .stdout(stdout)
                        .spawn();                // execute the command as a child process, return a handle to it.

                    match output {
                        Ok(output) => { previous_command = Some(output); },    // wait for the child to exit completely, returning the status it exited with.
                        Err(e) => {
                            previous_command = None;
                            eprintln!("{}", e);
                        }
                    };
                }
            }
        }

        if let Some(mut final_command) = previous_command {
            final_command.wait();
        }
    }
}
