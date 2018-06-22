#include <iostream>
#include <functional>

using std::placeholders::_1;

void callback(std::function<int(int)> f)
{
    f(30);
}

int main()
{
    int i = 100;

    callback(std::bind([&i](int x){ 
                std::cout << "value: " << x << ", " << i << std::endl;
                return 0;
                }, _1));

    return 0;
}
