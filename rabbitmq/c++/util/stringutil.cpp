#include "stringutil.h"
#include <stdio.h>
#include <string.h>

void trim(std::string& str)
{
    if (str.empty()) {
        return ;
    }

    str.erase(0, str.find_first_not_of(" "));
    str.erase(str.find_last_not_of(" ")+1);
}

std::string int2str(int i)
{
    char sz[32];
    memset(sz, 0, sizeof(sz));
    snprintf(sz, sizeof(sz), "%d", i);

    return sz; 
}
