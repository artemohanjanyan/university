#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
using namespace std;

const int maxn = 101;

int d[maxn][maxn];

int a[maxn];

int main()
{
	//ifstream cin("part2num.in");
	//ofstream cout("part2num.out");

	d[0][0] = 1;
	for (int sum = 0; sum < maxn; ++sum)
		for (int first = 1; first <= sum; ++first)
		{
			d[sum][first] += d[sum - first][0];
			for (int next = first; first + next <= sum; ++next)
				d[sum][first] += d[sum - first][next];
		}

	//for (int sum = 0; sum < 10; ++sum)
	//{
	//	for (int first = 0; first < 10; ++first)
	//		cerr << d[sum][first] << " ";
	//	cerr << endl;
	//}

	string expression;
	cin >> expression;

	stringstream in;
	in << expression;

	int sum = 0, n;
	for (n = 0; in >> a[n]; ++n)
	{
		sum += a[n];
		char plus;
		in >> plus;
	}

	int ans = 0, minFirst = 1;
	for (int i = 0; i < n; ++i)
	{
		for (int skipped = minFirst; skipped < a[i]; ++skipped)
			ans += d[sum][skipped];
		sum -= a[i];
		minFirst = a[i];
	}

	cout << ans << endl;

	return 0;
}
