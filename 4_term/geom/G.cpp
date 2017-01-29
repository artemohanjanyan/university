#include <bits/stdc++.h>
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

double exterior_product(vec2df const &a, vec2df const &b, vec2df const &c)
{
	return (c.x - a.x) * (b.y - a.y) - (c.y - a.y) * (b.x - a.x);
}

int left_turn(vec2df const &a, vec2df const &b, vec2df const &c)
{
	double v1 = exterior_product(a, b, c);
	if (v1 > 0)
		return 1;
	if (v1 < 0)
		return -1;

	return 0;
}

struct half_plane
{
	int a, b, c;
};

long long det(half_plane const &a, half_plane const &b, half_plane const &c)
{
	return ((long long) a.a) * b.b * c.c + ((long long) a.b) * b.c * c.a + ((long long) a.c) * b.a * c.b
	     - ((long long) a.c) * b.b * c.a - ((long long) a.b) * b.a * c.c - ((long long) a.a) * b.c * c.b;
}


bool compare(half_plane const& pl1, half_plane const& pl2)
{
	if ((pl1.b <= 0) != (pl2.b <= 0))
		return pl1.b < pl2.b;

	if (pl1.b == 0 && pl2.b == 0)
	{
		if ((pl1.a > 0) != (pl2.a > 0))
			return pl1.a > pl2.a;
		return pl1.c * abs(pl2.a) > pl2.c * abs(pl1.a);
	}

	int turn = left_turn(vec2d{0, 0}, {pl2.a, pl2.b}, {pl1.a, pl1.b});

	if (turn == 0)
		return det(pl1, {pl1.b, -pl1.a, 0}, pl2) > 0;

	return turn == -1;
}

long long det(half_plane const &a, half_plane const &b)
{
	return a.a * b.b - a.b * b.a;
}

int get_denom(half_plane const &a, half_plane const &b)
{
	return a.a * b.b - a.b * b.a;
}

vec2df intersect(half_plane const &a, half_plane const &b)
{
	int denom = get_denom(a, b);
	return {-double(a.c * b.b - b.c * a.b) / denom, -double(a.a * b.c - b.a * a.c) / denom};
}

bool is_point_in_half_plane(vec2df const &point, half_plane const &p)
{
	return point.x * p.a + point.y * p.b + p.c > 0;
}

// true if inf
bool parallel_half_planes_intersection(half_plane const &a, half_plane const &b)
{
	long long aa = abs(a.a);
	long long ab = abs(a.b);
	long long ac = a.c;

	long long ba = abs(b.a);
	long long bb = abs(b.b);
	long long bc = b.c;

	if (ba != 0)
		ac *= ba;
	if (bb != 0)
		ac *= bb;

	if (aa != 0)
		bc *= aa;
	if (ab != 0)
		bc *= ab;

	if (ac * (-1) < bc)
		return true;
	else
		return false;
}

bool operator<(half_plane const &a, half_plane const &b)
{
	return a.a < b.a || (a.a == b.a && (a.b < b.b || (a.b == b.b && (a.c < b.c))));
}

template<typename It, typename Pred>
void relax_collinear(It begin, Pred pred)
{
	for (It it = next(begin); pred(it); ++it)
		if ((begin->a == 0 && it->a == 0) ||
				(begin->a != 0 && it->a != 0 && begin->a * it->b == it->a * begin->b))
		{
			long long prod0 = begin->a;
			if (prod0 == 0)
				prod0 = begin->b;
			else if (begin->b != 0)
				prod0 *= begin->b;
			prod0 = abs(prod0);
			long long prodj = it->a;
			if (prodj == 0)
				prodj = it->b;
			else if (it->b != 0)
				prodj *= it->b;
			prodj = abs(prodj);
			if (it->c * it->c * prod0 <
					begin->c * begin->c * prodj)
				swap(*begin, *it);
		}
}

int main()
{
	int n;
	cin >> n;
	vector<half_plane> planes(n);
	for (int i = 0; i < n; ++i)
		cin >> planes[i].a >> planes[i].b >> planes[i].c;

	sort(planes.begin(), planes.end(), compare);

	//for (auto const &plane : planes)
	//	cerr << plane.a << " " << plane.b << " " << plane.c << "\n";

	vector<half_plane> planes1;
	size_t i;
	for (i = 0; i < planes.size() && planes[i].b <= 0; ++i)
	{
		if (planes[i].b == 0 && planes[i].a < 0)
			break;
		while (planes1.size() > 1 && det(planes1[planes1.size() - 2], planes1[planes1.size() - 1], planes[i]) >= 0)
			planes1.pop_back();
		planes1.push_back(planes[i]);
	}

	vector<half_plane> planes2;
	for (; i < planes.size(); ++i)
	{
		while (planes2.size() > 1 && det(planes2[planes2.size() - 2], planes2[planes2.size() - 1], planes[i]) >= 0)
			planes2.pop_back();
		planes2.push_back(planes[i]);
	}

	//cerr << "\n";
	//for (auto const &plane : planes1)
	//	cerr << plane.a << " " << plane.b << " " << plane.c << "\n";
	//cerr << "\n";
	//for (auto const &plane : planes2)
	//	cerr << plane.a << " " << plane.b << " " << plane.c << "\n";
	//cerr << "\n";

	if (planes1.size() == 0 || planes2.size() == 0)
	{
		cout << -1;
		return 0;
	}

	long long left_det = det(planes1.back(), planes2[0]);
	long long right_det = det(planes2.back(), planes1[0]);
	if (left_det > 0 || right_det > 0)
	{
		cout << -1;
		return 0;
	}
	if (left_det == 0)
	{
		relax_collinear(planes2.begin(),
				[&](vector<half_plane>::iterator it) -> bool
				{
					return it < planes2.end();
				});
		relax_collinear(planes1.rbegin(),
				[&](vector<half_plane>::reverse_iterator it) -> bool
				{
					return it < planes1.rend();
				});

		if (parallel_half_planes_intersection(planes1.back(), planes2[0]))
			cout << -1;
		else
			cout << 0;
		return 0;
	}
	if (right_det == 0)
	{
		relax_collinear(planes1.begin(),
				[&](vector<half_plane>::iterator it) -> bool
				{
					return it < planes1.end();
				});
		relax_collinear(planes2.rbegin(),
				[&](vector<half_plane>::reverse_iterator it) -> bool
				{
					return it < planes2.rend();
				});
		if (parallel_half_planes_intersection(planes2.back(), planes1[0]))
			cout << -1;
		else
			cout << 0;
		return 0;
	}

	planes = planes1;

	for (i = 0; i < planes2.size(); ++i)
	{
		while (planes.size() > 1 && det(planes[planes.size() - 2], planes[planes.size() - 1], planes2[i]) >= 0)
			planes.pop_back();
		planes.push_back(planes2[i]);
	}
	for (i = 0; i < planes1.size(); ++i)
	{
		while (planes.size() > 1 && det(planes[planes.size() - 2], planes[planes.size() - 1], planes1[i]) >= 0)
			planes.pop_back();
		planes.push_back(planes1[i]);
	}

	size_t start = numeric_limits<size_t>::max();
	map<half_plane, size_t> plane_map;
	for (i = 0; i < planes.size(); ++i)
	{
		if (plane_map.count(planes[i]))
		{
			start = plane_map[planes[i]];
			break;
		}
		plane_map[planes[i]] = i;
	}

	if (start == numeric_limits<size_t>::max())
	{
		cout << 0;
		return 0;
	}
	size_t end = i + 1;

	planes.push_back(planes[start]);
	vector<vec2df> points;
	for (i = start + 1; i < end; ++i)
		points.push_back(intersect(planes[i - 1], planes[i]));
	points.push_back(points[0]);

	double area = 0;
	for (i = 1; i < points.size(); ++i)
		area += (points[i].x - points[i - 1].x) * (points[i].y + points[i - 1].y);

	cout.precision(10);
	cout << fixed << area / 2;

	return 0;
}
