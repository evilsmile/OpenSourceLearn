// 左值与右值的概念其实在c++0x中就有了。概括地讲，凡是能够取地址的可能称之为左值，反之称为右值。
// 从其解决手段来看类似上面的定义，当然还可以定义为：有名字的对象为左值，没有名字的对象为右值。
//
// 折叠规则，保留参数原始类型，拒绝编译器的类型推导，以达到将参数完美转发到目的函数中
//
// 之所以存在完美转发，其问题实质是模板参数类型推导在转发过程中无法保证左右值的引用问题。
// 而完美转发就是在不破坏const属性的前提下通过增加左右值引用概念和新增参数推导规则解决这个问题。
// 
// 引用折叠规则就是函数接受参数形式与传入参数形式之间进行引用简化，具体编译器定义了这样一条规则：
//  1. T& + & => T&
//  2. T&& + & => T&
//  3. T& + && => T&
//  4. T&& + && => T&&
//  上面规则中，前者代表接受类型，后者代表进入类型，=>表示引用折叠之后的类型，即最后被推导决断的类型。
#include <iostream>
#include <string>

using namespace std;

template <typename T> 
struct Name;

template <>
struct Name<string>
{
    static const char * get()
    {
        return "string";
    }
};

template <>
struct Name<const string>
{ 
    static const char * get()
    {
        return "const string";
    }
};

template <>
struct Name<string&>
{ 
    static const char * get()
    {
        return "string&";
    }
};

template <>
struct Name<const string&>
{ 
    static const char * get()
    {
        return "const string&";
    }
};

template <>
struct Name<string&&>
{ 
    static const char * get()
    {
        return "string&&";
    }
};

template <>
struct Name<const string&&>
{ 
    static const char * get()
    {
        return "const string&&";
    }
};

template <typename T>
void quark(T&& t)
{
    cout << "*****************************" << endl;
    cout << "t: " << t << endl;
    cout << "T: " << Name<T>::get() << endl;
    cout << "T&&: " << Name<T&&>::get() << endl;
    cout << endl;
}

string strange()
{
    return "strange()";
}

const string charm()
{
    return "charm()";
}

int main()
{
    string up("up");
    const string down("down");

    // up 被推导为string&
    quark(up);
    quark(down);
    quark(strange());
    quark(charm());

    return 0;
}


