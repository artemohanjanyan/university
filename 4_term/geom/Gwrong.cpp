#include <iostream>
#include <vector>
#include <algorithm>
#include <cstdlib>
using namespace std;
struct _ { ios_base::Init i; _() { cin.sync_with_stdio(0); cin.tie(0); } } _;

struct vec2d
{
	int x, y;
};

int exterior_product(vec2d const &a, vec2d const &b, vec2d const &c)
{
	return (c.x - a.x) * (b.y - a.y) - (c.y - a.y) * (b.x - a.x);
}

int left_turn(vec2d const &a, vec2d const &b, vec2d const &c)
{
	int v1 = exterior_product(a, b, c);
	if (v1 > 0)
		return 1;
	if (v1 < 0)
		return -1;

	return 0;
}

struct vec2df
{
	double x, y;
};

struct half_plane
{
	int a, b, c;
};

bool operator==(const half_plane &a, const half_plane &b)
{
	return a.a == b.a && a.b == b.b && a.c == b.c;
}

long long det(half_plane const &a, half_plane const &b, half_plane const &c)
{
	return ((long long) a.a) * b.b * c.c + ((long long) a.b) * b.c * c.a + ((long long) a.c) * b.a * c.b
	     - ((long long) a.c) * b.b * c.a - ((long long) a.b) * b.a * c.c - ((long long) a.a) * b.c * c.b;
}

bool covered(half_plane const &a, half_plane const &b)
{
	return det(a, {a.b, -a.a, 0}, b) <= 0;
}

bool compare(half_plane const& pl1, half_plane const& pl2)
{
	if ((pl1.b <= 0) != (pl2.b <= 0))
		return pl1.b < pl2.b;

	if (pl1.b == 0 && pl2.b == 0)
	{
		if ((pl1.a > 0) != (pl2.a > 0))
			return pl1.a > pl2.a;
		//return pl1.c * abs(pl2.a) > pl2.c * abs(pl1.a);
		return covered(pl2, pl1);
	}

	int turn = left_turn(vec2d{0, 0}, {pl2.a, pl2.b}, {pl1.a, pl1.b});

	if (turn == 0)
		return covered(pl2, pl1);

	return turn == -1;
}

vec2df intersect(half_plane const &a, half_plane const &b)
{
	int denom = a.a * b.b - a.b * b.a;
	return {-double(a.c * b.b - b.c * a.b) / denom, -double(a.a * b.c - b.a * a.c) / denom};
}

int main()
{
	int n;
	cin >> n;
	vector<half_plane> planes(n);
	for (int i = 0; i < n; ++i)
		cin >> planes[i].a >> planes[i].b >> planes[i].c;

	sort(planes.begin(), planes.end(), compare);

	//cerr << "\n";
	//for (auto &plane : planes)
	//	cerr << plane.a << " " << plane.b << " " << plane.c << "\n";
	//cerr << "\n";

	for (size_t i = 0; i < planes.size(); ++i)
	{
		size_t j = i == 0 ? planes.size() - 1 : i - 1;
		int tmp = left_turn({0, 0}, {planes[j].a, planes[j].b}, {planes[i].a, planes[i].b});
		if (tmp < 0)
		{
			cout << -1;
			return 0;
		}
		if (tmp == 0 && planes[j].a * planes[i].a + planes[j].b * planes[i].b < 0)
		{
			if (det({-planes[i].a, -planes[i].b, -planes[i].c}, {-planes[i].b, planes[i].a, 0}, planes[j]) < 0)
				cout << -1;
			else
				cout << 0;
			return 0;
		}
		if (tmp == 0 && j == 0)
		{
			cout << -1;
			return 0;
		}
	}

	vector<half_plane> planes1;
	size_t i;
	for (i = 0; i < planes.size(); ++i)
	{
		while (planes1.size() > 1 && det(planes1[planes1.size() - 2], planes1[planes1.size() - 1], planes[i]) >= 0)
			planes1.pop_back();
		if (!planes1.empty())
		{
			int tmp = left_turn({0, 0}, {planes1.back().a, planes1.back().b}, {planes[i].a, planes[i].b});
			if (tmp < 0)
			{
				cout << 0;
				return 0;
			}
			if (tmp == 0)
			{
				if (planes1.back().a  * planes[i].a + planes1.back().b * planes[i].b > 0)
					planes1.pop_back();
				else
				{
					cout << 0;
					return 0;
				}
			}
		}
		planes1.push_back(planes[i]);
	}

	half_plane last_plane = planes1[0];
	while (planes1.size() > 1 && det(planes1[planes1.size() - 2], planes1[planes1.size() - 1], last_plane) >= 0)
		planes1.pop_back();
	if (!planes1.empty())
	{
		int tmp = left_turn({0, 0}, {planes1.back().a, planes1.back().b}, {last_plane.a, last_plane.b});
		if (tmp < 0)
		{
			cout << 0;
			return 0;
		}
		if (tmp == 0)
		{
			if (planes1.back().a  * last_plane.a + planes1.back().b * last_plane.b > 0)
				planes1.pop_back();
			else
			{
				cout << 0;
				return 0;
			}
		}
	}
	planes1.push_back(last_plane);

	//for (auto &plane : planes1)
	//	cout << plane.a << " " << plane.b << " " << plane.c << "\n";

	vector<vec2df> points;
	for (i = 1; i < planes1.size(); ++i)
		points.push_back(intersect(planes1[i - 1], planes1[i]));
	points.push_back(points[0]);

	double area = 0;
	for (i = 1; i < points.size(); ++i)
		area += (points[i].x - points[i - 1].x) * (points[i].y + points[i - 1].y);

	cout.precision(10);
	cout << fixed << area / 2;

	return 0;
}
