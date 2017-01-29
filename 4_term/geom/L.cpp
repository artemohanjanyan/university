#include <bits/stdc++.h>
using namespace std;

vector<pair<int, int>> points1;
vector<pair<int, int>> points2;

long long sqr(int a)
{
	return static_cast<long long>(a) * a;
}

double dist(pair<int, int> const &a, pair<int, int> const &b)
{
	return sqrt(sqr(a.first - b.first) + sqr(a.second - b.second));
}

pair<int, int> vec(pair<int, int> const &a, pair<int, int> const &b)
{
	return {b.first - a.first, b.second - a.second};
}

long long scalar(pair<int, int> const &a, pair<int, int> const &b)
{
	return static_cast<long long>(a.first) * b.first + static_cast<long long>(a.second) * b.second;
}

double segment_dist(pair<int, int> const &a, pair<int, int> const &b, pair<int, int> const &c)
{
	if (scalar(vec(a, b), vec(a, c)) > 0 && scalar(vec(b, a), vec(b, c)) > 0)
	{
		//double ac = dist(a, c);
		//double bc = dist(b, c);
		//double p = (ab + ac + bc) / 2;
		//double s = sqrt(p * (p - ab) * (p - ac) * (p - bc));
		long long s2 = static_cast<long long>(b.first - a.first) * (c.second - a.second)
				- static_cast<long long>(b.second - a.second) * (c.first - a.first);
		if (s2 < 0)
			s2 = -s2;
		if (s2 != 0)
			return static_cast<double>(s2) / dist(a, b);
	}
	
	return min(dist(a, c), dist(b, c));
}

int main()
{
	int n1;
	cin >> n1;
	points1.resize(n1);
	for (int i = 0; i < n1; ++i)
		cin >> points1[i].first >> points1[i].second;

	int n2;
	cin >> n2;
	points2.resize(n2);
	for (int i = 0; i < n2; ++i)
		cin >> points2[i].first >> points2[i].second;

	int i2 = 0;
	double min_dist = dist(points1[0], points2[0]);
	for (int i = 0; i < n1; ++i)
	{
		while (true)
		{
			double cur_dist = dist(points1[i], points2[i2]);

			int i3 = ((i2 - 1) + n2) % n2;
			double new_dist = dist(points1[i], points2[i3]);
			if (new_dist < cur_dist)
			{
				i2 = i3;
				continue;
			}

			i3 = (i2 + 1) % n2;
			new_dist = dist(points1[i], points2[i3]);
			if (new_dist < cur_dist)
			{
				i2 = i3;
				continue;
			}
			else
				break;
		}

		int il = (i - 1 + n1) % n1;
		int ir = (i + 1) % n1;
		min_dist = min(min_dist, segment_dist(points1[il], points1[i], points2[i2]));
		min_dist = min(min_dist, segment_dist(points1[ir], points1[i], points2[i2]));

		int i2l = (i2 - 1 + n2) % n2;
		int i2r = (i2 + 1) % n2;
		min_dist = min(min_dist, segment_dist(points2[i2], points2[i2l], points1[i]));
		min_dist = min(min_dist, segment_dist(points2[i2], points2[i2r], points1[i]));

		//cerr << i << " " << i2 << "\n";
		//cerr << il << " " << segment_dist(points1[il], points1[i], points2[i2]) << "\n";
		//cerr << points1[il].first << " " << points1[il].second << "\n";
		//cerr << points1[i].first << " " << points1[i].second << "\n";
		//cerr << points2[i2].first << " " << points2[i2].second << "\n";
		////cerr << ir << " " << segment_dist(points1[ir], points1[i], points2[i2]) << "\n";

		////cerr << i2l << " " << segment_dist(points2[i2], points2[i2l], points1[i]) << "\n";
		////cerr << i2r << " " << segment_dist(points2[i2], points2[i2r], points1[i]) << "\n";
		//cerr << "\n";
	}

	cout.precision(20);
	cout << min_dist << "\n";

	return 0;
}
