#include <bits/stdc++.h>
using namespace std;
struct _ { ios_base::Init i; _() { cin.sync_with_stdio(0); cin.tie(0); } } _;

struct vec
{
	int x, y;
};

bool comp(vec const &a, vec const &b)
{
	return a.x < b.x || (a.x == b.x && a.y < b.y);
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

vector<vec> points;
vector<vec> hull1;
vector<vec> hull2;

int main()
{
	int n;
	cin >> n;
	points.resize(n);
	for (int i = 0; i < n; ++i)
		cin >> points[i].x >> points[i].y;

	sort(points.begin(), points.end(), comp);

	hull1.push_back(points[0]);
	for (int i = 1; i < (int) points.size(); ++i)
	{
		while (hull1.size() > 1 && left_turn(hull1[hull1.size() - 2], hull1[hull1.size() - 1], points[i]) >= 0)
			hull1.pop_back();
		hull1.push_back(points[i]);
	}

	hull2.push_back(points[points.size() - 1]);
	for (int i = (int) points.size() - 2; i >= 0; --i)
	{
		while (hull2.size() > 1 && left_turn(hull2[hull2.size() - 2], hull2[hull2.size() - 1], points[i]) >= 0)
			hull2.pop_back();
		hull2.push_back(points[i]);
	}

	cout << hull1.size() + hull2.size() - 2 << "\n";
	for (int i = 1; i < (int) hull1.size(); ++i)
		cout << hull1[i].x << " " << hull1[i].y << "\n";
	for (int i = 1; i < (int) hull2.size(); ++i)
		cout << hull2[i].x << " " << hull2[i].y << "\n";

	return 0;
}
