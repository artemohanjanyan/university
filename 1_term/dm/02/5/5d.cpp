#include <bits/stdc++.h>
using namespace std;
typedef long long LL;
struct _ { ios_base::Init i; _() { cin.sync_with_stdio(0); cin.tie(0); } } _;

int const maxn = 31;

LL pascal[maxn][maxn];

int a[maxn];

int main()
{
	ifstream cin("choose2num.in");
	ofstream cout("choose2num.out");

	pascal[0][0] = 1;
	for (int i = 1; i < maxn; ++i)
	{
		pascal[i][0] = pascal[i - 1][0];
		for (int j = 1; j <= i; ++j)
			pascal[i][j] = pascal[i - 1][j - 1] + pascal[i - 1][j];
	}

	LL n, k;
	cin >> n >> k;
	for (int i = 0; i < k; ++i)
		cin >> a[i];

	LL m = 0;
	int cur = 0;
	for (int next = 1; k > 0; ++next)
		if (a[cur] != next)
		{
			m += pascal[n - 1][k - 1];
			--n;
		}
		else
		{
			--n;
			--k;
			++cur;
		}

	cout << m << endl;

	return 0;
}
