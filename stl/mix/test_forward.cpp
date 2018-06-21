// 右值引用(Rvalue Reference)是C++新标准C++11中引入的新特性，它实现了转移语义(Move sementics)和精确传递(PerfectForwarding)。
// 它的主要目的有两个方面：
// 1. 消除两个对象交互时不必要的对象拷贝，节省运算存储资源，提高效率
// 2. 能够更简洁明确地定义泛型函数
//
#include <iostream>
#include <memory>
//forward
#include <utility>
#include <array>

struct A {
    A(int&& n) { std::cout << "rvalue overload, n=" << n << "\n"; }
    A(int& n) { std::cout << "lvalue overload, n=" << n << "\n"; }
};

class B {
public:
    template<class T1, class T2, class T3>
        B(T1&& t1, T2&& t2, T3&& t3) : 
            a1_ {std::forward<T1>(t1)},
            a2_ {std::forward<T2>(t2)},
            a3_ {std::forward<T3>(t3)}
    {
    }

private:
    A a1_, a2_, a3_;
};

template <class T, class U>
std::unique_ptr<T> make_unique1(U&& u)
{
    return std::unique_ptr<T>(new T(std::forward<U>(u)));
}

template <class T, class... U>
std::unique_ptr<T> make_unique(U&&... u)
{
    return std::unique_ptr<T>(new T(std::forward<U>(u)...));
}

int main()
{
    auto p1 = make_unique1<A>(2);   // right value
    int i = 1;
    auto p2 = make_unique1<A>(i);   // left value

    std::cout << "B\n";
    auto t = make_unique<B>(2, i, 3);
}
