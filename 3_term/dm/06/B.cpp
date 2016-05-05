#include <bits/stdc++.h>
using namespace std;

int const maxn = 100;
int const maxm = 1000;

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

int main()
{
	ifstream cin("mincost.in");
	ofstream cout("mincost.out");

	int n, m;
	cin >> n >> m;
	for (int i = 0; i < m; ++i)
	{
		int from, to, cap, cost;
		cin >> from >> to >> cap >> cost;
		--from, --to;
		edges[i * 2] = {from, to, cap, cost, 0};
		edges[i * 2 + 1] = {to, from, 0, -cost, 0};
		graph[from].push_back(i * 2);
		graph[to].push_back(i * 2 + 1);
	}

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

	cout << ans;

	return 0;
}
