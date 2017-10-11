#include "func.h"
#include <gtest/gtest.h>

TEST(AlgorithmTest, twoValues) 
{
    EXPECT_EQ(3, add(1, 2));
    EXPECT_EQ(1, sub(5, 4));
}

TEST(CmpTest, twoValues) 
{
    EXPECT_EQ(2, max(1, 2));
    EXPECT_EQ(2, min(2, 4));
}
