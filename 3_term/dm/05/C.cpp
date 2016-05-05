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

bool used[maxn];
void dfs1(int v)
{
	used[v] = true;
	for (int i = 0; i < (int) a[v].size(); ++i)
	{
		int ei = a[v][i];
		if (!used[edges[ei].to] && edges[ei].c > 0 && edges[ei].c - edges[ei].f > 0)
			dfs1(edges[ei].to);
	}
}

int main()
{
	ifstream cin("cut.in");
	ofstream cout("cut.out");

	cin >> n >> m;
	for (int i = 0; i < m; ++i)
	{
		int from, to, c;
		cin >> from >> to >> c;
		add_edge(from - 1, to - 1, c);
		add_edge(to - 1, from - 1, c);
	}

	s = 0;
	t = n - 1;

	while (bfs())
		while (dfs());

	//cerr << endl;
	//for (int i = 0; i < n; ++i)
	//{
	//	for (int j = 0; j < (int) a[i].size(); ++j)
	//	{
	//		int ei = a[i][j];
	//		cerr << "(" << edges[ei].from << " " << edges[ei].to << " " << edges[ei].c << " " << edges[ei].f << ") ";
	//	}
	//	cerr << endl;
	//}
	//cerr << endl;

	dfs1(0);
	cout << count(used, used + n, true) << "\n";
	for (int i = 0; i < n; ++i)
		if (used[i])
			cout << i + 1 << " ";

	return 0;
}
