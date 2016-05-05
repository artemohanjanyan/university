#include <bits/stdc++.h>
using namespace std;
struct _ { ios_base::Init i; _() { cin.sync_with_stdio(0); cin.tie(0); } } _;

int const maxn = 100000;
vector<int> graph[maxn];

bool used[maxn], fl;
int colors[maxn];
void dfs(int v, int color)
{
	used[v] = true;
	colors[v] = color;
	for (int to : graph[v])
		if (!used[to])
			dfs(to, color ^ 1);
		else if (colors[v] == colors[to])
			fl = true;
}

int main()
{
	ifstream cin("bipartite.in");
	ofstream cout("bipartite.out");

	int n, m;
	cin >> n >> m;
	for (int i = 0; i < m; ++i)
	{
		int from, to;
		cin >> from >> to;
		--from, --to;
		graph[from].push_back(to);
		graph[to].push_back(from);
	}

	for (int i = 0; i < n; ++i)
		if (!used[i])
			dfs(i, 0);

	if (fl)
		cout << "NO\n";
	else
		cout << "YES\n";

	return 0;
}
