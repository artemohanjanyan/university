#include <bits/stdc++.h>
using namespace std;

int const maxn = 20000;
vector<pair<int, int>> graph[maxn];
vector<int> bridges;

bool used[maxn];
int tin[maxn], tup[maxn], curTime;
void dfs(int v, int p = -1)
{
	used[v] = true;
	tin[v] = tup[v] = curTime++;
	for (auto to : graph[v])
		if (!used[to.first])
		{
			dfs(to.first, v);
			tup[v] = min(tup[v], tup[to.first]);
			if (tin[v] < tup[to.first])
				bridges.push_back(to.second);
		}
		else if (to.first != p)
			tup[v] = min(tup[v], tin[to.first]);
}

int main()
{
	ifstream cin("bridges.in");
	ofstream cout("bridges.out");

	int n, m;
	cin >> n >> m;
	for (int i = 0; i < m; ++i)
	{
		int from, to;
		cin >> from >> to;
		--from, --to;
		graph[from].push_back(make_pair(to, i + 1));
		graph[to].push_back(make_pair(from, i + 1));
	}

	for (int i = 0; i < n; ++i)
		if (!used[i])
			dfs(i);

	sort(bridges.begin(), bridges.end());
	cout << bridges.size() << "\n";
	for (int i : bridges)
		cout << i << " ";

	return 0;
}
