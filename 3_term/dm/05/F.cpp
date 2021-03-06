#include <bits/stdc++.h>

using namespace std;

struct _ { ios_base::Init i; _() { cin.sync_with_stdio(0); cin.tie(0); } } _;

const int maxn = 500;

class Edge
{
	public:
		int from, to, c, f;
		Edge(int ifrom, int ito, int ic)
		{
			from = ifrom;
			to = ito;
			c = ic;
			f = 0;
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

int main()
{
	ifstream cin("circulation.in");
	ofstream cout("circulation.out");

	cin >> n >> m;
	n += 2;
	s = 0;
	t = n - 1;
	for (int i = 0; i < m; ++i)
	{
		int from, to, lc, uc;
		cin >> from >> to >> lc >> uc;
		add_edge(s, to, lc);
		add_edge(from, to, uc - lc);
		add_edge(from, t, lc);
	}

	while (bfs())
		while (dfs());

	for (int i = 0; i < (int) a[0].size(); ++i)
	{
		int ei = a[0][i];
		if (edges[ei].f < edges[ei].c)
		{
			cout << "NO\n";
			return 0;
		}
	}

	cout << "YES\n";
	for (int i = 0; i < (int) edges.size(); i += 6)
		cout << edges[i + 2].f + edges[i + 4].f << endl;

	return 0;
}
