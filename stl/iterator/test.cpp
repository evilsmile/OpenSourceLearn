#include <iostream>

template <typename T>
struct MyIter {
    typedef typename T::value_type value_type;
};

template <typename T>
struct MyIter<T*> {
    typedef T value_type;
};

template <typename T>
void s()
{
    typename MyIter<T*>::value_type v;
}


struct A_tag {
};
struct B_tag {
};

struct A_iter {
    typedef A_tag iterator_category;
};
struct B_iter {
    typedef B_tag iterator_category;
};

template <typename Iterator>
struct iterator_traits {
    typedef typename Iterator::iterator_category iterator_category;
};

template <typename T>
void advance(T t, A_tag)
{
    std::cout << "A_tag" << std::endl;
}
template <typename T>
void advance(T t, B_tag)
{
    std::cout << "B_tag" << std::endl;
}

template <typename T>
void advance(T t)
{
    advance(typename iterator_traits<T>::iterator_category());
}

int main()
{
    s<MyIter<int*> >();

    A_iter a;
    advance(a);

	return 0;
}
