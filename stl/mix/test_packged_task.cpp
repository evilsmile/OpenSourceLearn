#include <iostream>
#include <future>
#include <chrono>
#include <functional>
#include <cmath>

int Test_Fun(int a, int b, int& c)
{
    std::this_thread::sleep_for(std::chrono::seconds(5));

    c = a + b + 230;

    return c;
}

int f(int x, int y) 
{
    return std::pow(x, y);
}

void task_lambda()
{
    std::packaged_task<int(int, int)> task([](int a, int b) {
            return std::pow(a, b);
    });
    std::future<int> result = task.get_future();

    task(2, 9);

    std::cout << "task_lambda:\t" << result.get() << std::endl;
}

void task_bind()
{
    std::packaged_task<int()> task(std::bind(f, 2, 11));
    std::future<int> result = task.get_future();
    task();

    std::cout << "task_bind:\t" << result.get() << std::endl;
}

void task_thread()
{
    std::packaged_task<int(int,int)> task(f);
    std::future<int> result = task.get_future();
    std::thread task_td(std::move(task), 2, 10);
    task_td.join();

    std::cout << "task_thread:\t" << result.get() << std::endl;
}

void test()
{
    std::packaged_task<int(int, int, int&)> pt1(Test_Fun);
    std::future<int> fu1 = pt1.get_future();

    int c = 0;

    std::thread t1(std::move(pt1), 1, 2, std::ref(c));

    int iRet = fu1.get();

    std::cout << "Result: " << iRet << std::endl;
    std::cout << "c: " << c << std::endl;
}

int main()
{
    task_lambda();
    task_bind();
    task_thread();

    return 0;
}
