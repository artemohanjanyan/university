#include <bits/stdc++.h>
using namespace std;

int const maxn = 100000;

vector<int> graph[maxn];
int color[maxn];
bool isCycle;
vector<int> cycle;
bool cycleFinished;
int endI;

void dfs(int v)
{
	color[v] = 1;
	for (int to : graph[v])
		if (color[to] == 0)
		{
			dfs(to);
			if (isCycle && !cycleFinished)
			{
				cycle.push_back(v);
				if (endI == v)
					cycleFinished = true;
				return;
			}
			else if (isCycle)
				return;
		}
		else if (color[to] == 1)
		{
			isCycle = true;
			endI = to;
			cycleFinished = false;
			cycle.push_back(v);
			return;
		}
	color[v] = 2;
}

int main()
{
	ifstream cin("cycle.in");
	ofstream cout("cycle.out");

	int n, m;
	cin >> n >> m;
	for (int i = 0; i < m; ++i)
	{
		int from, to;
		cin >> from >> to;
		--from, --to;
		graph[from].push_back(to);
	}

	for (int i = 0; i < n && !isCycle; ++i)
		if (color[i] == 0)
			dfs(i);

	if (!isCycle)
		cout << "NO";
	else
	{
		cout << "YES\n";
		reverse(cycle.begin(), cycle.end());
		for (int v : cycle)
			cout << v + 1 << " ";
	}

	return 0;
}
