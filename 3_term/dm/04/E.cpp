#include <bits/stdc++.h>
using namespace std;

using EdgeType = long long;
using Graph = vector<vector<pair<int, EdgeType>>>;

void dfs0(int v, Graph& g, vector<int>& colors, vector<int>& order)
{
	colors[v] = 1;

	for (auto& to : g[v])
		if (colors[to.first] == -1 && to.second == 0)
			dfs0(to.first, g, colors, order);

	order.push_back(v);
}

void dfs(int v, Graph& g, vector<int>& colors, int& color)
{
	colors[v] = color;

	for (auto& to : g[v])
		if (colors[to.first] == -1 && to.second == 0)
			dfs(to.first, g, colors, color);
}

void dfs1(int v, Graph& g, vector<int>& colors)
{
	colors[v] = 1;

	for (auto& to : g[v])
		if (colors[to.first] == -1)
			dfs1(to.first, g, colors);
}

Graph condensation(Graph& src)
{
	static vector<int> colors(src.size()), order(src.size());
	colors.resize(src.size());
	fill(colors.begin(), colors.end(), -1);
	order.resize(0);
	Graph reversed(src.size());
	for (int i = 0; i < (int) src.size(); ++i)
		for (auto& to : src[i])
			if (to.second == 0)
				reversed[to.first].push_back(make_pair(i, 0));
	for (int i = 0; i < (int) reversed.size(); ++i)
		if (colors[i] == -1)
			dfs0(i, reversed, colors, order);

	int color = -1;
	fill(colors.begin(), colors.end(), -1);
	for (auto it = order.rbegin(); it != order.rend(); ++it)
		if (colors[*it] == -1)
			dfs(*it, src, colors, ++color);

	Graph condensed(color + 1);

	++color;
	int rootColor = colors[0];
	if (rootColor != 0)
	{
		replace(colors.begin(), colors.end(), rootColor, color);
		replace(colors.begin(), colors.end(), 0, rootColor);
		replace(colors.begin(), colors.end(), color, 0);
	}

	for (int i = 0; i < (int) src.size(); ++i)
		for (auto& to : src[i])
			if (colors[i] != colors[to.first])
				condensed[colors[i]].push_back(make_pair(colors[to.first], to.second));

	return move(condensed);
}

int main()
{
	ifstream cin("chinese.in");
	ofstream cout("chinese.out");

	int n;
	cin >> n;
	Graph g(n);
	int m;
	cin >> m;
	for (int i = 0; i < m; ++i)
	{
		int from, to, w;
		cin >> from >> to >> w;
		--from, --to;
		g[from].push_back(make_pair(to, w));
	}

	EdgeType ans = 0;

	vector<EdgeType> minEdges(g.size());
	minEdges[0] = 0;
	vector<int> colors(g.size());

	bool errorFl = false;

	fill(colors.begin(), colors.end(), -1);
	dfs1(0, g, colors);
	if (find(colors.begin(), colors.end(), -1) != colors.end())
		errorFl = true;

	while (!errorFl)
	{
		minEdges.resize(g.size());
		colors.resize(g.size());
		fill(next(minEdges.begin()), minEdges.end(), numeric_limits<EdgeType>::max());

		for (int i = 0; i < (int) g.size(); ++i)
			for (auto& to : g[i])
				minEdges[to.first] = min(minEdges[to.first], to.second);

		for (int i = 0; i < (int) g.size(); ++i)
			ans += minEdges[i];
		for (int i = 0; i < (int) g.size(); ++i)
			for (auto& to : g[i])
				to.second -= minEdges[to.first];

		fill(colors.begin(), colors.end(), -1);
		int color = 0;

		dfs(0, g, colors, color);
		if (find(colors.begin(), colors.end(), -1) == colors.end())
			break;

		g = move(condensation(g));
	}

	if (errorFl)
		cout << "NO\n";
	else
		cout << "YES\n" << ans << "\n";

	return 0;
}
