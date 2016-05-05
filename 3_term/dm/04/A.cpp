#include <bits/stdc++.h>
using namespace std;

using Point = pair<int, int>;

int getDist(Point const &a, Point const &b)
{
	return (a.first - b.first) * (a.first - b.first) + (a.second - b.second) * (a.second - b.second);
}

int const maxn = 5000;

Point points[maxn];
bool used[maxn];
int minD[maxn];

int main()
{
	ifstream cin("spantree.in");
	ofstream cout("spantree.out");

	int n;
	cin >> n;
	for (int i = 0; i < n; ++i)
		cin >> points[i].first >> points[i].second;

	fill(minD + 1, minD + n, numeric_limits<int>::max());
	minD[0] = 0;
	double ans = 0;

	for (int edgeN = 0; edgeN < n; ++edgeN)
	{
		int minTo = 0, minDist = numeric_limits<int>::max();
		for (int i = 0; i < n; ++i)
			if (!used[i] && minDist > minD[i])
			{
				minTo = i;
				minDist = minD[i];
			}

		used[minTo] = true;
		ans += sqrt((double) minDist);

		for (int i = 0; i < n; ++i)
			if (!used[i] && getDist(points[minTo], points[i]) < minD[i])
				minD[i] = getDist(points[minTo], points[i]);
	}

	cout.precision(18);
	cout << fixed << ans << endl;

	return 0;
}
