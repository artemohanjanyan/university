#include <bits/stdc++.h>
using namespace std;

int const maxn = 20000;
vector<int> graph[maxn];
vector<int> points;

bool used[maxn];
int tin[maxn], tup[maxn], curTime;
void dfs(int v, int p = -1)
{
	used[v] = true;
	tin[v] = tup[v] = curTime++;
	int c = 0;
	for (int to : graph[v])
		if (!used[to])
		{
			dfs(to, v);
			tup[v] = min(tup[v], tup[to]);
			if (p != -1 && tin[v] <= tup[to])
				points.push_back(v);
			++c;
		}
		else if (to != p)
			tup[v] = min(tup[v], tin[to]);
	if (p == -1 && c > 1)
		points.push_back(v);
}

int main()
{
	ifstream cin("points.in");
	ofstream cout("points.out");

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
			dfs(i);

	sort(points.begin(), points.end());
	points.resize(unique(points.begin(), points.end()) - points.begin());
	cout << points.size() << "\n";
	for (int i : points)
		cout << i + 1 << "\n";

	return 0;
}
