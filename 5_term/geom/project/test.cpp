#include <gtest/gtest.h>

#include "geometry.h"

TEST(geometry, turn)
{
    ASSERT_EQ(turn({0, 0, 1}, {1, 0, 1}, {1, 1, 1}), left);
    ASSERT_EQ(turn({0, 0, 1}, {1, 1, 1}, {1, 0, 1}), right);
    ASSERT_EQ(turn({0, 0, 1}, {1, 1, 1}, {2, 2, 1}), collinear);
}

TEST(geometry, contains)
{
    ASSERT_TRUE(contains({0, 0, 1}, {{{-1, -1, 1}, {1, 0, 1}, {0, 1, 1}}}));
    ASSERT_FALSE(contains({-1, 1, 1}, {{{-1, -1, 1}, {1, 0, 1}, {0, 1, 1}}}));
    ASSERT_TRUE(contains({-1, 0, 2}, {{{-1, -1, 1}, {1, 0, 1}, {0, 1, 1}}}));
}

TEST(geometry, contains_inf)
{
    ASSERT_TRUE(contains({-1, 1, 0}, {{{-1, -1, 1}, {1, 1, 1}, {0, 0, 0}}}));
    ASSERT_FALSE(contains({-1, 1, 0}, {{{1, 1, 1}, {-1, -1, 1}, {0, 0, 0}}}));
}

TEST(geometry, ray_segment_intersection)
{
    ASSERT_TRUE(has_intersection({{-1, 0, 1}, {1, 0, 1}}, {{0, -2, 1}, {0, -1, 1}}));
    ASSERT_FALSE(has_intersection({{-1, 0, 1}, {1, 0, 1}}, {{0, -1, 1}, {0, -2, 1}}));
    ASSERT_FALSE(has_intersection({{-1, 0, 1}, {1, 0, 1}}, {{0, -2, 1}, {1, -1, 1}}));
}
