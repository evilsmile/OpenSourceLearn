// 使用容器的时候，尽量用value_type是个好习惯
// typedef T value_type;
#include <iostream>
#include <string>
#include <map>
#include <vector>

using namespace std;


int main()
{
    typedef map<string, string> info_t;
    typedef vector<int> ints_t;
    info_t m;
    m["name"] = "ian";
    m["age"] = "18";

    for (info_t::value_type& v : m) {
        std::cout << v.first << " : " << v.second << std::endl;
    }

    ints_t some_ints;
    some_ints.push_back(1);
    some_ints.push_back(2);
    // change value directly
    for (ints_t::value_type& v : some_ints) {
        v++;
    }
    for (ints_t::value_type& v : some_ints) {
        std::cout << v << std::endl;
    }


    
    return 0;
}
