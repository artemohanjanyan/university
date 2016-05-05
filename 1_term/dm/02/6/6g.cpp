#include <bits/stdc++.h>
using namespace std;
typedef long long LL;
struct _ { ios_base::Init i; _() { cin.sync_with_stdio(0); cin.tie(0); } } _;

const int maxn = 100001;

int a[maxn];

int main()
{
	ifstream cin("nextpartition.in");
	ofstream cout("nextpartition.out");

	int sum;
	cin >> sum;
	char tmp;
	int n;
	for (n = 0; cin >> tmp >> a[n]; ++n);

	int curSum = 0;
	for (int i = n - 1; i >= 0; --i)
	{
		if (curSum > 0)
		{
			if (a[i] + 1 <= curSum - 1)
			{
				++a[i];
				--curSum;
			}
			else
			{
				a[i] += curSum;
				curSum = 0;
			}

			for (n = i + 1; curSum > 0; ++n)
				if (curSum - a[i] >= a[i])
				{
					a[n] = a[i];
					curSum -= a[i];
				}
				else
				{
					a[n] = curSum;
					curSum = 0;
				}

			cout << sum << "=" << a[0];
			for (int j = 1; j < n; ++j)
				cout << "+" << a[j];

			return 0;
		}

		curSum += a[i];
	}

	cout << "No solution\n";

	return 0;
}
