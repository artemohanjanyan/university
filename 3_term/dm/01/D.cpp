#include <bits/stdc++.h>
using namespace std;
struct _ { ios_base::Init i; _() { cin.sync_with_stdio(0); cin.tie(0); } } _;

int const maxn = 100000;

vector<int> graph[maxn];

int color[maxn];

void dfs(int v)
{
	color[v] = 1;
	for (int to : graph[v])
	{
		if (color[to] == 0)
			dfs(to);
		if (color[to] == 1)
			color[v] = 2;
	}
}

int main()
{
	ifstream cin("game.in");
	ofstream cout("game.out");

	int n, m, s;
	cin >> n >> m >> s;
	--s;
	for (int i = 0; i < m; ++i)
	{
		int from, to;
		cin >> from >> to;
		--from, --to;
		graph[from].push_back(to);
	}

	dfs(s);

	cout << (color[s] == 1 ? "Second" : "First");
	cout << " player wins";

	return 0;
}
