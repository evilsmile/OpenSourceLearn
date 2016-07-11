#include <iostream>
#include <vector>

template <class Arg, class Result>
struct unary_function {
    typedef Arg argument_type;
    typedef Result result_type;
};
template <class Arg1,class Arg2, class Result>
struct binary_function {
    typedef Arg1 fisrt_argument_type;
    typedef Arg2 second_argument_type;
    typedef Result result_type;
};

template <class Number>
struct event : public unary_function<Number, std::vector<int>::iterator>
{
    bool operator()(Number x) { return (x&1) == 0; }
};

template <class Iterator, class Predict>
typename Predict::result_type
find_if(Iterator begin, Iterator end, Predict f)
{
    typename Predict::result_type result = end;
    while (begin != end && !(f(*begin))) {
        begin++;
    }
    if (begin != end) {
        result = begin;
    }
    return result;
}

int main()
{
    std::vector<int> v(4);
    v[0] = 1;
    v[1] = 3;
    v[2] = 2;
    v[3] = 1;

    std::vector<int>::iterator iter 
        = find_if(v.begin(), v.end(), event<int>());
    if (iter == v.end()) {
        std::cout << "Not found!" << std::endl;
    } else {
        std::cout << "Find even " << *iter << std::endl;
    }


    return 0;
}
