#include <iostream>
#include <fstream>
#include <string>
using namespace std;

const int maxn = 101;

int d[maxn][maxn];

int main()
{
	//ifstream cin("num2part.in");
	//ofstream cout("num2part.out");

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

	int n, r;
	cin >> n >> r;

	int minFirst = 1;
	string ans;

	while (n > 0)
		for (int acc = 0, first = minFirst; first <= n; acc += d[n][first], ++first)
			if (acc + d[n][first] > r)
			{
				n -= first;
				r -= acc;
				minFirst = first;

				ans += to_string(first) + "+";

				break;
			}

	ans.erase(--ans.end());
	cout << ans << endl;

	return 0;
}
