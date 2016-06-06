#include <bits/stdc++.h>
using namespace std;

int const maxn = 2000;
int const maxm = 200000;

struct Edge
{
	int from, to, cap, cost, flow;
};
Edge edges[maxm * 2];

int dist[maxn];
int used[maxn];
int parent[maxn];
queue<int> q;

vector<int> graph[maxn];

int edgeNum = -1;
void addEdge(int from, int to, int cap, int cost)
{
	++edgeNum;
	edges[edgeNum * 2] = {from, to, cap, cost, 0};
	edges[edgeNum * 2 + 1] = {to, from, 0, -cost, 0};
	graph[from].push_back(edgeNum * 2);
	graph[to].push_back(edgeNum * 2 + 1);
}

map<int, int> order[maxn];

int main()
{
	ifstream cin("rsumc.in");
	ofstream cout("rsumc.out");

	int n, m;
	cin >> n >> m;
	for (int i = 0; i < n; ++i)
	{
		addEdge(0, i + 1, 1, 0);

		for (int j = 0; j < m; ++j)
		{
			int p;
			cin >> p;
			for (int k = 0; k < n; ++k)
				addEdge(i + 1, (j * n + k) + n + 1, 1, p * (k + 1));
		}
	}
	for (int j = 0; j < m; ++j)
		for (int k = 0; k < n; ++k)
			addEdge((j * n + k) + n + 1, 1 + n + n * m, 1, 0);
	int oldN = n;
	n = 1 + n + n * m + 1;

	long long ans = 0;

	while (true)
	{
		fill(dist + 1, dist + n, numeric_limits<int>::max());
		dist[0] = 0;

		fill(used + 1, used + n, false);
		used[0] = true;

		fill(parent, parent + n, -1);
		q = queue<int>{};
		q.push(0);
		while (!q.empty())
		{
			int cur = q.front();
			q.pop();
			used[cur] = false;
			for (auto edgeI : graph[cur])
				if (edges[edgeI].flow < edges[edgeI].cap &&
						dist[cur] + edges[edgeI].cost < dist[edges[edgeI].to])
				{
					dist[edges[edgeI].to] = dist[cur] + edges[edgeI].cost;
					parent[edges[edgeI].to] = edgeI;
					if (!used[edges[edgeI].to])
					{
						q.push(edges[edgeI].to);
						used[edges[edgeI].to] = true;
					}
				}
		}

		if (dist[n - 1] == numeric_limits<int>::max())
			break;

		int pathFlow = numeric_limits<int>::max();
		int curV = n - 1;
		while (parent[curV] != -1)
		{
			pathFlow = min(pathFlow, edges[parent[curV]].cap - edges[parent[curV]].flow);
			curV = edges[parent[curV]].from;
		}

		curV = n - 1;
		while (parent[curV] != -1)
		{
			edges[parent[curV]].flow += pathFlow;
			edges[parent[curV] ^ 1].flow -= pathFlow;
			ans += ((long long) edges[parent[curV]].cost) * pathFlow;
			curV = edges[parent[curV]].from;
		}
	}

	cout << ans << "\n";

	for (int i = 0; i < edgeNum * 2; ++i)
		if (edges[i].from > 0 && edges[i].from <= oldN && edges[i].to > oldN && edges[i].flow == 1)
			order[(edges[i].to - oldN - 1) / oldN][n - (edges[i].to - oldN - 1) % oldN] = edges[i].from;

	for (int i = 0; i < m; ++i)
	{
		cout << order[i].size() << " ";
		for (auto &p : order[i])
			cout << p.second << " ";
		cout << "\n";
	}

	return 0;
}
