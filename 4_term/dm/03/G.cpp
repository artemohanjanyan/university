#include <bits/stdc++.h>
using namespace std;
struct _ { ios_base::Init i; _() { cin.sync_with_stdio(0); cin.tie(0); } } _;

int const MAXN = 1000;
int const MAXTIME = 100 * MAXN;

int p[2][MAXN];

int dp[MAXTIME + 1], tmp[MAXTIME + 1];

int main()
{
	freopen("r2cmax.in", "r", stdin);
	freopen("r2cmax.out", "w", stdout);
	ios_base::sync_with_stdio(false);
	cin.tie(NULL);

	int n;
	cin >> n;
	for (int i = 0; i < 2; ++i)
		for (int j = 0; j < n; ++j)
			cin >> p[i][j];

	int max_time = 0;
	for_each(p[0], p[0] + n, [&](int x){ max_time += x; });

	fill(dp + 1, dp + max_time + 1, INT_MAX);

	for (int i = 0; i < n; ++i)
	{
		for (int j = 0; j <= max_time; ++j)
			tmp[j] = min(p[0][i] <= j ? dp[j - p[0][i]] : INT_MAX,
			             dp[j] != INT_MAX ? dp[j] + p[1][i] : INT_MAX);
		swap(dp, tmp);
	}

	for (int j = 0; j <= max_time; ++j)
		dp[j] = max(dp[j], j);

	cout << *min_element(dp, dp + max_time + 1);

	return 0;
}
