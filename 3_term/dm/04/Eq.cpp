#include <bits/stdc++.h>
using namespace std;

using ll = long long;

int const maxn = 1000;

vector<pair<int, ll>> graph[maxn];
vector<pair<int, ll>> graph1[maxn];

int colors[maxn];

void dfs1(int v)
{
	colors[v] = 0;
	for (auto& to : graph[v])
		if (colors[to.first] == -1)
			dfs1(to.first);
}

vector<int> order;
void dfs2(int v)
{
	colors[v] = 0;
	for (auto& to : graph[v])
		if (to.second == 0 && colors[to.first] == -1)
			dfs2(to.first);
	order.push_back(v);
}

vector<int> reversed[maxn];

void dfs3(int v, int color)
{
	colors[v] = color;
	for (auto& to : reversed[v])
		if (colors[to] == -1)
			dfs3(to, color);
}

ll minEdge[maxn];

int main()
{
	ifstream cin("chinese.in");
	ofstream cout("chinese.out");

	int n, m;
	cin >> n >> m;
	for (int i = 0; i < m; ++i)
	{
		int from, to, w;
		cin >> from >> to >> w;
		--from, --to;
		graph[from].push_back(make_pair(to, w));
	}

	fill(colors, colors + n, -1);
	dfs1(0);
	if (find(colors, colors + n, -1) != colors + n)
	{
		cout << "NO\n";
		return 0;
	}

	ll ans = 0;
	int root = 0;
	while (true)
	{
		fill(minEdge, minEdge + n, numeric_limits<ll>::max());
		for (int i = 0; i < n; ++i)
			for (auto& to : graph[i])
				minEdge[to.first] = min(minEdge[to.first], to.second);

		for (int i = 0; i < n; ++i)
			if (i != root)
				ans += minEdge[i];

		for (int i = 0; i < n; ++i)
			for (auto& to : graph[i])
				if (to.first != root)
					to.second -= minEdge[to.first];

		fill(colors, colors + n, -1);
		order.clear();
		dfs2(root);
		if ((int) order.size() == n)
			break;

		order.clear();
		fill(colors, colors + n, -1);
		for (int i = 0; i < n; ++i)
			if (colors[i] == -1)
				dfs2(i);

		fill(reversed, reversed + n, vector<int>());
		for (int i = 0; i < n; ++i)
			for (auto& to : graph[i])
				if (to.second == 0)
					reversed[to.first].push_back(i);

		int color = 0;
		fill(colors, colors + n, -1);
		for (auto it = order.rbegin(); it != order.rend(); ++it)
			if (colors[*it] == -1)
				dfs3(*it, color++);

		root = colors[root];
		fill(graph1, graph1 + color, vector<pair<int, ll>>());
		for (int i = 0; i < n; ++i)
			for (auto& to : graph[i])
				if (colors[i] != colors[to.first])
					graph1[colors[i]].push_back(make_pair(colors[to.first], to.second));

		swap(graph, graph1);
		n = color;
	}

	cout << "YES\n" << ans << "\n";

	return 0;
}
