#include <bits/stdc++.h>

using namespace std;

struct _ { ios_base::Init i; _() { cin.sync_with_stdio(0); cin.tie(0); } } _;

const int maxn = 500;

int count1 = 0;

class Edge
{
	public:
		int from, to, c, f;
		int edgeI;
		Edge(int ifrom, int ito, int ic)
		{
			from = ifrom;
			to = ito;
			c = ic;
			f = 0;
			edgeI = ++count1;
		}
};

vector<Edge> edges;
vector<int> a[maxn];
int n, m, s, t;

void add_edge(int from, int to, int c)
{
	a[from].push_back((int) edges.size());
	edges.push_back(Edge(from, to, c));

	a[to].push_back((int) edges.size());
	edges.push_back(Edge(to, from, 0));
}

int d[maxn], Q[maxn], ql, qr;
int ptr[maxn];
bool bfs()
{
	fill(d, d + n, -1);
	ql = qr = 0;
	Q[qr++] = s;
	d[s] = 0;

	while (ql < qr && d[t] == -1)
	{
		int cur = Q[ql++];

		for (size_t i = 0; i < a[cur].size(); ++i)
		{
			int ei = a[cur][i];
			if (d[edges[ei].to] == -1 && edges[ei].c > edges[ei].f)
			{
				d[edges[ei].to] = d[cur] + 1;
				Q[qr++] = edges[ei].to;
			}
		}
	}

	fill(ptr, ptr + n, 0);
	return d[t] != -1;
}

int dfs(int v = s, int flow = INT_MAX)
{
	if (v == t || flow == 0)
		return flow;

	for (; ptr[v] < (int) a[v].size(); ++ptr[v])
	{
		int ei = a[v][ptr[v]];
		Edge &edge = edges[ei];

		if (d[v] + 1 != d[edge.to])
			continue;

		int pushed = dfs(edge.to, min(flow, edge.c - edge.f));
		if (pushed)
		{
			edge.f += pushed;
			edges[ei ^ 1].f -= pushed;
			return pushed;
		}
	}

	return 0;
}

int colors[maxn], color;
vector<int> curPath;
int dfs1(int v, int minFlow = INT_MAX)
{
	colors[v] = color;
	if (v == t)
		return minFlow;

	for (int i = 0; i < (int) a[v].size(); ++i)
	{
		int ei = a[v][i];
		if (colors[edges[ei].to] == color)
			continue;

		if (edges[ei].f > 0)
		{
			int res = dfs1(edges[ei].to, min(minFlow, edges[ei].f));
			if (res != 0)
			{
				edges[ei].f -= res;
				curPath.push_back(edges[ei].edgeI);
				return res;
			}
		}
	}

	return 0;
}

int main()
{
	ifstream cin("decomposition.in");
	ofstream cout("decomposition.out");

	cin >> n >> m;
	for (int i = 0; i < m; ++i)
	{
		int from, to, c;
		cin >> from >> to >> c;
		add_edge(from - 1, to - 1, c);
	}

	s = 0;
	t = n - 1;

	while (bfs())
		while (dfs());

	vector<pair<int, vector<int>>> ans;

	++color;
	while (int res = dfs1(s))
	{
		reverse(curPath.begin(), curPath.end());
		ans.push_back(make_pair(res, curPath));
		curPath.clear();
		++color;
	}

	cout << ans.size() << "\n";
	for (auto& path : ans)
	{
		cout << path.first << " " << path.second.size() << " ";
		for (int ei : path.second)
			cout << ei / 2 + 1 << " ";
		cout << "\n";
	}

	return 0;
}
