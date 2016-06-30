#include <iostream>
#include <string>
#include <sstream>
#include <limits>

std::string doubleToString(double d)
{
    std::ostringstream str;
    str.imbue(std::locale::classic());
    str.precision(std::numeric_limits<double>::digits10 + 1);
    str << d;
    return str.str();
}

int main(int argc, char* argv[])
{
    double d = 12.34567f;

    std::cout << doubleToString(d) << std::endl;

	return 0;
}

