#include "tests.h"
#include <stddef.h>
#include <iostream>
#include <limits>
#include <gmpxx.h>
#include <boost/numeric/interval.hpp>

using namespace std;
using namespace boost::numeric;
using namespace boost::numeric::interval_lib::compare::certain;

template<typename T>
struct vec2d
{
	T x, y;
};

template<typename T>
bool operator<(vec2d<T> const &a, vec2d<T> const &b)
{
	return a.x < b.x || a.x == b.x && a.y < b.y;
}
template<typename T>
bool operator==(vec2d<T> const &a, vec2d<T> const &b)
{
	return a.x == b.x && a.y == b.y;
}
template<typename T>
bool operator<=(vec2d<T> const &a, vec2d<T> const &b)
{
	return a < b || a == b;
}
template<typename T>
bool operator>(vec2d<T> const &a, vec2d<T> const &b)
{
	return a.x > b.x || a.x == b.x && a.y > b.y;
}

using vec2dD = vec2d<double>;
using vec2dI = vec2d<interval<double>>;
using vec2dG = vec2d<mpq_class>;

template<typename T>
T exterior_product(vec2d<T> const &a, vec2d<T> const &b, vec2d<T> const &c)
{
	return (c.x - a.x) * (b.y - a.y) - (c.y - a.y) * (b.x - a.x);
}

const double eps8 = numeric_limits<double>::epsilon();
int left_turn(vec2dD const &a, vec2dD const &b, vec2dD const &c)
{
	double v1 = exterior_product(a, b, c);
	double eps = eps8 * abs((c.x - a.x) * (b.y - a.y)) + abs((c.y - a.y) * (b.x - a.x));
	if (v1 > eps)
		return 1;
	if (v1 < -eps)
		return -1;

	interval<double> v2 = exterior_product<interval<double>>(vec2dI{a.x, a.y}, vec2dI{b.x, b.y}, vec2dI{c.x, c.y});
	//if (cergt(v2, 0.0))
	//	return 1;
	//if (cerlt(v2, 0.0))
	//	return -1;

	if (v2 > 0.0)
		return 1;
	if (v2 < 0.0)
		return -1;

	mpq_class v3 = exterior_product(vec2dG{a.x, a.y}, vec2dG{b.x, b.y}, vec2dG{c.x, c.y});
	if (v3 > 0)
		return 1;
	if (v3 < 0)
		return -1;

	return 0;
}

bool between(vec2dD x, vec2dD l, vec2dD r)
{
	return l <= x && x <= r;
}

int main()
{
	int testId;
	cin >> testId;
	vector<double> ans = genTest(testId);

	for (int i = 0; i < (int) ans.size(); i += 8)
	{
		vec2dD a{ans[i], ans[i+1]}, b{ans[i+2], ans[i+3]}, c{ans[i+4], ans[i+5]}, d{ans[i+6], ans[i+7]};

		int t1 = left_turn(a, b, c);
		int t2 = left_turn(a, b, d);
		int t3 = left_turn(c, d, a);
		int t4 = left_turn(c, d, b);

		bool intersect = t1 != t2 && t3 != t4;
		if (intersect)
			cout << 'Y';
		else if (t1 == 0 && t2 == 0 && t3 == 0 && t4 == 0)
		{
			if (a > b)
				swap(a, b);
			if (c > d)
				swap(c, d);

			intersect = between(c, a, b) || between(d, a, b) || between(a, c, d) || between(b, c, d);
			cout << (intersect ? 'Y' : 'N');
		}
		else
			cout << 'N';
	}

	return 0;
}
