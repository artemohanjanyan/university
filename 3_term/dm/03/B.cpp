#include <bits/stdc++.h>
using namespace std;

using ll = long long;

int const maxn = 2000;

ll inf = numeric_limits<ll>::max();

ll graph[maxn][maxn];
bool used[maxn];
ll length[maxn];

int main()
{
	freopen("pathmgep.in", "r", stdin);
	freopen("pathmgep.out", "w", stdout);

	int n, s, f;
	scanf("%d%d%d", &n, &s, &f);
	--s, --f;

	for (int i = 0; i < n; ++i)
		for (int j = 0; j < n; ++j)
			scanf("%lld", &graph[i][j]);

	fill(length, length + n, inf);
	length[s] = 0;

	while (!used[f])
	{
		int cur = -1;
		ll curl = inf;
		for (int i = 0; i < n; ++i)
			if (!used[i] && length[i] < curl)
			{
				cur = i;
				curl = length[i];
			}

		if (curl == inf)
			break;
		used[cur] = true;

		for (int to = 0; to < n; ++to)
			if (graph[cur][to] != -1 && !used[to])
				length[to] = min(length[to], curl + graph[cur][to]);
	}

	if (!used[f])
		printf("-1\n");
	else
		printf("%lld\n", length[f]);

	return 0;
}
