#include <iostream>

class A {
public:
    A() : array(new int[3]{1,2,3})
    {
    }

    ~A()
    {
        if (nullptr != array)
        {
            delete [] array;
        }
    }

    A(A&& a)
    {
        array = a.array;
        a.array = nullptr;
        std::cout << "Memory Copy constructor" << std::endl;
    }

public:
    int *array{nullptr};
};

void ProcessValue(int& i)
{
    std::cout << "LValue processed: " << i << std::endl;
}

void ProcessValue(int&& i) 
{
    std::cout << "RValue processed: " << i << std::endl;
}

int main()
{
    A a1;
    // 左值引用转换为右值引用
    A a2(std::move(a1));

    std::cout << a1.array << std::endl;
    std::cout << a2.array << std::endl;

    int a = 0;
    ProcessValue(a);
    // 如果已知一个命名对象不再被使用而想对它调用转移构造函数和转移赋值函数，
    // 也就是把一个左值引用当作右值引用来使用，
    // 可以使用std::move. 下面当左值引用转移为右值引用
    ProcessValue(std::move(a));

    return 0;
}
