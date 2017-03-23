#include <bits/stdc++.h>
using namespace std;
struct _ { ios_base::Init i; _() { cin.sync_with_stdio(0); cin.tie(0); } } _;

double const EPS = 0.0000001;

struct point
{
	double x, y;
};

double sqr(double a)
{
	return a * a;
}

double dist(point const &a, point const &b)
{
	return sqrt(sqr(a.x - b.x) + sqr(a.y - b.y));
}

struct circle
{
	point center_;
	double radius_;

	circle(point const &center, double radius)
			: center_(center)
			, radius_(radius)
	{}

	circle(point const &a, point const &b, point const &c)
	{
		double bc = b.y - c.y;
		double ca = c.y - a.y;
		double ab = a.y - b.y;
		double d = 2 * (a.x * bc + b.x * ca + c.x * ab);
		center_.x =
				(a.x * a.x + a.y * a.y) * bc +
				(b.x * b.x + b.y * b.y) * ca +
				(c.x * c.x + c.y * c.y) * ab;
		center_.y =
				(a.x * a.x + a.y * a.y) * (c.x - b.x) +
				(b.x * b.x + b.y * b.y) * (a.x - c.x) +
				(c.x * c.x + c.y * c.y) * (b.x - a.x);
		center_.x /= d;
		center_.y /= d;

		radius_ = dist(a, center_);
	}

	bool contains(point const &p)
	{
		return dist(center_, p) < radius_ + EPS;
	}
};

vector<point> s;

circle min_disk_with_point(size_t n, point const &q);
circle min_disk_with_2_points(size_t n, point const &q1, point const &q2);

circle min_disk()
{
	random_shuffle(s.begin(), s.end());
	point middle;
	middle.x = (s[0].x + s[1].x) / 2;
	middle.y = (s[0].y + s[1].y) / 2;
	circle d{middle, dist(middle, s[0])};
	for (size_t i = 2; i < s.size(); ++i)
		if (!d.contains(s[i]))
			d = min_disk_with_point(i, s[i]);
	return d;
}

circle min_disk_with_point(size_t n, point const &q)
{
	random_shuffle(s.begin(), s.begin() + n);
	point middle;
	middle.x = (s[0].x + q.x) / 2;
	middle.y = (s[0].y + q.y) / 2;
	circle d{middle, dist(middle, s[0])};
	for (size_t i = 1; i < n; ++i)
		if (!d.contains(s[i]))
			d = min_disk_with_2_points(i, s[i], q);
	return d;
}

circle min_disk_with_2_points(size_t n, point const &q1, point const &q2)
{
	point middle;
	middle.x = (q1.x + q2.x) / 2;
	middle.y = (q1.y + q2.y) / 2;
	circle d{middle, dist(middle, q1)};
	for (size_t i = 0; i < n; ++i)
		if (!d.contains(s[i]))
			d = circle(s[i], q1, q2);
	return d;
}

int main()
{
	int n;
	cin >> n;
	s.resize(n);
	for (int i = 0; i < n; ++i)
		cin >> s[i].x >> s[i].y;

	srand(313141);

	circle ans = n == 1 ? circle(s[0], 0) : min_disk();

	cout.precision(20);
	cout << ans.radius_ << "\n";
	cout << ans.center_.x << " " << ans.center_.y << "\n";

	return 0;
}
