/*
 * 在C++中 union 无用处，因为union的成员只能是POD类型
 * boost中提供了variant实现类似的功能
 */
#include <boost/variant.hpp>
#include <string>
#include <iostream>

using namespace boost;

typedef variant<int, double, std::string> var_t;

// 访问者模式访问variant
// 需要继承于static_visitor，并重载()操作符
// 再通过apply_visitor来访问原始类型的值
class print_visitor : public boost::static_visitor<void>
{
    public:
        void operator()(int i) const {
            std::cout << "Int: " << i << "\n";
        }
        void operator()(double d) const {
            std::cout << "Double: " << d << "\n";
        }
        void operator()(const std::string& s) const {
            std::cout << "String: " << s << "\n";
        }
};

int main()
{
    print_visitor pv;
    var_t var;

    var = "variant demo";
    boost::apply_visitor(pv, var);
    var = 4.14;
    boost::apply_visitor(pv, var);
    var = 823;
    boost::apply_visitor(pv, var);

    return 0;
}
