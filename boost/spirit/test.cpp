#include <vector>

#include <boost/assign.hpp>

#include <boost/spirit/version.hpp>

#if SPIRIT_VERSION <= 0x1806
#include <boost/spirit/core.hpp>
#include <boost/spirit/attribute.hpp>
namespace spirit_ns = boost::spirit;
#else
#include <boost/spirit/home/classic/core.hpp>
#include <boost/spirit/home/classic/attribute.hpp>
namespace spirit_ns = boost::spirit::classic;
#endif
//#include <boost/spirit/include/classic.hpp>
//#include <boost/spirit/include/qi.hpp>
//#include <boost/spirit/include/phoenix_core.hpp>
//#include <boost/spirit/include/phoenix_object.hpp>

std::vector<int> g_output;

int on_parse_int(const int val)
{
    g_output.push_back(val);
}

int main()
{
    spirit_ns::rule<> int_csv_rule = spirit_ns::int_p[on_parse_int] >> *(',' >> spirit_ns::int_p[on_parse_int]);
    spirit_ns::parse("2,3,4", int_csv_rule);

    return 0;
}
