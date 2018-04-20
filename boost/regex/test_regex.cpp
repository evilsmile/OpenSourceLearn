#include <boost/regex.hpp>
#include <string>
#include <iostream>

int main()
{
    //std::string str_regex = "((a)bc)d(efg)";
    std::string str_test = "smsp_access: 0 , 380 | smsp_c2s: 1, 20 ";
    //std::string str_test = "smsp_access: 0 , 380 |smsp_c2s:1,20 ";
    //std::string str_regex = "([0-9_a-b]+:[0-9 ]*,[ 0-9])([0-9_a-b]+:[ 0-9]*,[ 0-9]\\|)*";
    std::string str_regex = "([0-9_a-zA-Z]+:[0-9 ]*,[0-9 ]*)(\\| *[0-9_a-zA-Z]+:[0-9 ]*,[0-9 ]*)*";
    
    boost::regex re(str_regex);

    boost::smatch m;
    if (boost::regex_match(str_test, m, re)) 
    {
        std::cout << "Matched" << std::endl;
        for (int i = 0; i < m.size(); ++i) {
            std::cout << "[" << i << "]: " << m[i] << std::endl;
        }
    } else {
        std::cout << "NOT Matched" << std::endl;
    }

    return 0;
}
