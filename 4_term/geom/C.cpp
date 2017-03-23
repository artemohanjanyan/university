#include <bits/stdc++.h>
using namespace std;
struct _ { ios_base::Init i; _() { cin.sync_with_stdio(0); cin.tie(0); } } _;

int const MAX_N = 100000;

struct vec
{
	int x, y;
};

long long exterior_product(vec const &a, vec const &b, vec const &c)
{
	return static_cast<long long>(b.x - a.x) * (c.y - a.y) -
			static_cast<long long>(b.y - a.y) * (c.x - a.x);
}

int left_turn(vec const &a, vec const &b, vec const &c)
{
	long long pr = exterior_product(a, b, c);
	return (pr > 0) - (pr < 0);
}

bool between(int a, int b, int c)
{
	return min(a, b) <= c && c <= max(a, b);
}

bool between(vec const &a, vec const &b, vec const &c)
{
	return between(a.x, b.x, c.x) && between(a.y, b.y, c.y);
}

vec points[MAX_N];

int main()
{
	size_t n;
	cin >> n;
	for (size_t i = 0; i < n; ++i)
		cin >> points[i].x >> points[i].y;

	for (size_t i = 2; i < n; ++i)
		if (int sign = left_turn(points[i - 2], points[i - 1], points[i]))
		{
			if (sign == 1)
				reverse(points, points + n);
			break;
		}

	size_t test_n;
	cin >> test_n;
	for (size_t test_i = 0; test_i < test_n; ++test_i)
	{
		vec point;
		cin >> point.x >> point.y;
		
		if (left_turn(points[0], points[1], point) == 1 ||
				left_turn(points[n - 1], points[0], point) == 1)
		{
			cout << "OUTSIDE\n";
			continue;
		}

		// binary search
		// predicate(i) = left_turn(points[0], points[i], point) != -1;
		size_t l = 0, r = n - 1;
		while (l + 1 < r)
		{
			size_t mid = l + (r - l) / 2;
			if (left_turn(points[0], points[mid], point) != -1)
				r = mid;
			else
				l = mid;
		}

		if (r == 1)
		{
			if (between(points[0], points[1], point))
				cout << "BORDER\n";
			else
				cout << "OUTSIDE\n";
			continue;
		}
		if (r == n - 1 && left_turn(points[0], points[n - 1], point) == 0)
		{
			if (between(points[0], points[n - 1], point))
				cout << "BORDER\n";
			else
				cout << "OUTSIDE\n";
			continue;
		}

		int last_turn = left_turn(points[r], points[l], point);
		if (last_turn == 1)
			cout << "INSIDE\n";
		else if (last_turn == 0)
			cout << "BORDER\n";
		else
			cout << "OUTSIDE\n";
	}

	return 0;
}
