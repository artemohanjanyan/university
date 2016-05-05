#include <bits/stdc++.h>
using namespace std;
typedef long long LL;
struct _ { ios_base::Init i; _() { cin.sync_with_stdio(0); cin.tie(0); } } _;

const int maxn = 5000;

int a[maxn], d[maxn], p[maxn], ans[maxn];

int main()
{
	ifstream cin("lis.in");
	ofstream cout("lis.out");

	int n;
	cin >> n;
	for (int i = 0; i < n; ++i)
		cin >> a[i];

	d[0] = 1;
	p[0] = -1;
	int maxi = 0;

	for (int i = 1; i < n; ++i)
	{
		d[i] = 1;
		p[i] = -1;

		for (int j = i - 1; j >= 0; --j)
			if (a[i] > a[j])
				if (d[i] < d[j] + 1)
				{
					p[i] = j;
					d[i] = d[j] + 1;
				}

		if (d[i] > d[maxi])
			maxi = i;
	}

	int i = maxi, ansi = d[maxi] - 1;
	while (i != -1)
	{
		ans[ansi--] = a[i];
		i = p[i];
	}

	cout << d[maxi] << "\n";
	for (int j = 0; j < d[maxi]; ++j)
		cout << ans[j] << " ";
	cout << endl;

	return 0;
}
