#include <bits/stdc++.h>
using namespace std;
struct _ { ios_base::Init i; _() { cin.sync_with_stdio(0); cin.tie(0); } } _;

int const MAX_N = 100000;

struct vec
{
	int x, y;
};

bool operator<(vec const &a, vec const &b)
{
	return a.x < b.x || (a.x == b.x && a.y < b.y);
}

bool operator==(vec const &a, vec const &b)
{
	return a.x == b.x && a.y == b.y;
}

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

std::vector<pair<vec, vec>> segments;

int main()
{
	vec check_point;
	size_t n;
	cin >> n >> check_point.x >> check_point.y;

	vec start_point;
	cin >> start_point.x >> start_point.y;
	vec prev_point = start_point;
	for (size_t i = 1; i < n; ++i)
	{
		vec point;
		cin >> point.x >> point.y;
		segments.push_back(make_pair(prev_point, point));
		if (!(segments.back().first < segments.back().second))
			swap(segments.back().first, segments.back().second);
		prev_point = point;		
	}
	segments.push_back(make_pair(prev_point, start_point));
	if (!(segments.back().first < segments.back().second))
		swap(segments.back().first, segments.back().second);

	int intersections = 0;

	for (auto &segment : segments)
	{
		vec A = segment.first;
		vec B = segment.second;

		if (check_point == A || check_point == B)
		{
			cout << "YES\n";
			return 0;
		}

		if (A.x == check_point.x && B.x == check_point.x)
		{
			if (A.y <= check_point.y && check_point.y <= B.y)
			{
				cout << "YES\n";
				return 0;
			}
			continue;
		}

		if (!(A.x <= check_point.x && check_point.x < B.x))
			continue;

		int turn = left_turn(check_point, A, B);
		if (turn == 0)
		{
			cout << "YES\n";
			return 0;
		}

		intersections += (turn == -1);
	}

	cout << (intersections % 2 == 0 ? "NO" : "YES") << "\n";

	return 0;
}
