#include <iostream>

using namespace std;

template <class... T>
void f(T... args)
{
    // 打印个数
    cout << sizeof...(args) << endl;
}

// -------------------- 通过终止递归函数展开参数包 -----------------
// 终止递归函数
void print()
{
    cout << "empty" << endl;
}

// 递归函数
template <class T, class ...Args>
void print(T head, Args... rest)
{
    cout << "parameter " << head << endl;
    print(rest...);
}


template <typename T>
T sum(T t)
{
    return t;
}

template <typename T, typename ... Types>
T sum(T first, Types... rest)
{
    return first + sum<T>(rest...);
}

// -----------------------------------
template <class F, class... Args>
void expand(const F& f, Args&&... args)
{
    initializer_list<int>{(f(std::forward<Args>(args)),0)...};
}

int main()
{
    f();
    f(1,2);

    print(1,2,3,4);

    cout << "sum is: " << sum(1,2,3,4) << endl;

    expand([](int i){cout << i << endl;}, 1, 2,3);

    return 0;
}
