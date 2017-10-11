#include "func.h"
#include <gtest/gtest.h>

#include <iostream>

TEST(CalcTest, twoValues) 
{
    Calculator cal; 

    EXPECT_EQ(3, cal.add(1, 2));
    EXPECT_EQ(1, cal.sub(5, 4));
}

TEST(CmpTest, twoValues) 
{
    Comparison cmp;
    EXPECT_EQ(2, cmp.max(1, 2));
    EXPECT_EQ(2, cmp.min(2, 4));
}
