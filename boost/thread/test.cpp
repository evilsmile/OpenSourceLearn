#include <boost/thread/thread.hpp>
#include <boost/thread/mutex.hpp>

#include <iostream>

boost::mutex io_mutex;

struct Count {
    Count(int id) : _id(id) {}

    void operator()() {
        for (int i = 0; i < 10; ++i) {
            boost::mutex::scoped_lock lock(io_mutex);
            std::cout << _id << ": " << i << std::endl;
        }
    }

    int _id;
};

int main()
{
    boost::thread thread1(Count(1));
    boost::thread thread2(Count(2));
    thread1.join();
    thread2.join();

    char c;
//    std::cin >> c;

    return 0;
}
