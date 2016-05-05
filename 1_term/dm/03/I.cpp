#include <bits/stdc++.h>
using namespace std;
typedef long long LL;
struct _ { ios_base::Init i; _() { cin.sync_with_stdio(0); cin.tie(0); } } _;

const int maxn = 18;

int edges[maxn][maxn];
int weights[maxn][maxn];

int dp[maxn][1 << maxn];

int main()
{
	ifstream cin("salesman.in");
	ofstream cout("salesman.out");

	int n, m;
	cin >> n >> m;
	for (int i = 0; i < m; ++i)
	{
		int from, to, weight;
		cin >> from >> to >> weight;
		--from, --to;

		edges[from][to] = edges[to][from] = 1;
		weights[from][to] = weights[to][from] = weight;
	}

	int lastMask = 1 << n;

	for (int i = 0; i < n; ++i)
		fill(dp[i], dp[i] + lastMask, INT_MAX);
	for (int i = 0; i < n; ++i)
		dp[i][0] = 0;

	for (int mask = 0; mask < lastMask; ++mask)
		for (int last = 0; last < n; ++last)
			if (!(mask & (1 << last)))
				for (int preLast = 0; preLast < n; ++preLast)
				{
					int preMask = mask ^ (1 << preLast);

					if ((mask & (1 << preLast)) && dp[preLast][preMask] != INT_MAX && edges[last][preLast])
						if (dp[last][mask] > dp[preLast][preMask] + weights[last][preLast])
							dp[last][mask] = dp[preLast][preMask] + weights[last][preLast];
				}

	int ans = INT_MAX;
	for (int i = 0; i < n; ++i)
		ans = min(ans, dp[i][(lastMask - 1) ^ (1 << i)]);

	if (ans != INT_MAX)
		cout << ans;
	else
		cout << -1;

	return 0;
}
