#include "func.h"

int Calculator::add(int a, int b)
{
    return a + b;
}

int Calculator::sub(int a, int b)
{
    return a - b;
}

int Comparison::max(int a, int b)
{
    return a>b?a:b;
}

int Comparison::min(int a, int b)
{
    return a<b?a:b;
}
