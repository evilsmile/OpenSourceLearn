#include <iostream>
#include <functional>
#include <thread>
#include <future>
#include <unistd.h>

using namespace std;

void print_int(std::future<int>& fut) 
{
    cout << "future waiting..." << endl;
    fut.wait();
    cout << "future wait finished." << endl;
    int x = fut.get();
    cout << "value: " << x << endl;
}

int main()
{
    // A promise is an object that can store a value of type T to be retrived by a future object, 
    // offering a synchronization point.
    // The promise object is the asynchronous provider and is expected to set a value for the shared state at some point
    // The future object is an asynchronous return object that can retrieve the value of the shared state, waiting for it to be ready, if necessary
    promise<int> prom;
    future<int> fut = prom.get_future();
    thread t(print_int, std::ref(fut));

    sleep(4);
    prom.set_value(10);
    t.join();

    return 0;
}
