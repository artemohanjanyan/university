#include <bits/stdc++.h>
using namespace std;

int const maxn = 100000;
vector<int> graph[maxn];

bool used[maxn];
int colors[maxn];
void dfs(int v, int color)
{
	used[v] = true;
	colors[v] = color;
	for (int to : graph[v])
		if (!used[to])
			dfs(to, color);
}

int main()
{
	ifstream cin("components.in");
	ofstream cout("components.out");

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

	int curColor = 0;
	for (int i = 0; i < n; ++i)
		if (!used[i])
			dfs(i, curColor++);

	cout << curColor << "\n";
	for (int i = 0; i < n; ++i)
		cout << colors[i] + 1 << " ";

	return 0;
}
