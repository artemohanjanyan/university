#include <bits/stdc++.h>
using namespace std;

int const maxn = 1000;
int const maxm = 300000;

struct Edge
{
	int from, to, cap, cost, flow;
};
Edge edges[maxm * 2];

int dist[maxn];
bool used[maxn];
int parent[maxn];
queue<int> q;

vector<int> graph[maxn];

bool edgeUsed[maxm];

int main()
{
	ifstream cin("assignment.in");
	ofstream cout("assignment.out");

	int n;
	cin >> n;
	int sink = n * 2 + 1;
	int edgeCount = 0;
	for (int i = 0; i < n; ++i)
		for (int j = 0; j < n; ++j)
		{
			int cost;
			cin >> cost;
			int from = i + 1, to = n + j + 1;
			edges[edgeCount] = {from, to, 1, cost, 0};
			edges[edgeCount + 1] = {to, from, 0, -cost, 0};
			graph[from].push_back(edgeCount);
			graph[to].push_back(edgeCount + 1);
			edgeCount += 2;
		}
	for (int i = 0; i < n; ++i)
	{
		edges[edgeCount] = {0, i + 1, 1, 0, 0};
		edges[edgeCount + 1] = {i + 1, 0, 0, 0, 0};
		graph[0].push_back(edgeCount);
		graph[i + 1].push_back(edgeCount + 1);
		edgeCount += 2;

		edges[edgeCount] = {n + i + 1, sink, 1, 0, 0};
		edges[edgeCount + 1] = {sink, n + i + 1, 0, 0, 0};
		graph[n + i + 1].push_back(edgeCount);
		graph[sink].push_back(edgeCount + 1);
		edgeCount += 2;
	}

	int oldN = n;
	n = n * 2 + 2;

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
			for (int edgeI : graph[cur])
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

	n = oldN;

	fill(used, used + n, false);
	for (int j = 0; j < n; ++j)
		for (int edgeI : graph[j + 1])
			if (edges[edgeI].flow == 1)
			{
				cout << j + 1 << " " << edges[edgeI].to - oldN << "\n";
				edges[edgeI].flow = 0;
				break;
			}

	return 0;
}
