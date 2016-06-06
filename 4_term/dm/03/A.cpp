#include <bits/stdc++.h>

using namespace std;

struct _ { ios_base::Init i; _() { cin.sync_with_stdio(0); cin.tie(0); } } _;

const int maxn = 100000;

double const eps = 0.0000001;

class Edge
{
	public:
		int from, to;
		double c, f;
		Edge(int ifrom, int ito, double ic)
		{
			from = ifrom;
			to = ito;
			c = ic;
			f = 0;
		}
};

vector<Edge> edges;
vector<int> a[maxn];
int n, s, t;

void add_edge(int from, int to, double c)
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
			if (d[edges[ei].to] == -1 && abs(edges[ei].c - edges[ei].f) > eps)
			{
				d[edges[ei].to] = d[cur] + 1;
				Q[qr++] = edges[ei].to;
			}
		}
	}

	fill(ptr, ptr + n, 0);
	return d[t] != -1;
}

double dfs(int v = s, double flow = 1e10)
{
	if (v == t || abs(flow) < eps)
		return flow;

	for (; ptr[v] < (int) a[v].size(); ++ptr[v])
	{
		int ei = a[v][ptr[v]];
		Edge &edge = edges[ei];

		if (d[v] + 1 != d[edge.to])
			continue;

		double pushed = dfs(edge.to, min(flow, edge.c - edge.f));
		if (abs(pushed) > eps)
		{
			edge.f += pushed;
			edges[ei ^ 1].f -= pushed;
			return pushed;
		}
	}

	return 0;
}

int const MAXNN = 1000;
double pp[MAXNN], rr[MAXNN], dd[MAXNN];
double ss[MAXNN];

int main()
{
	ifstream cin("cheese.in");
	ofstream cout("cheese.out");

	int nn, mm;
	double sumpp = 0;
	cin >> nn >> mm;
	for (int i = 0; i < nn; ++i)
	{
		cin >> pp[i] >> rr[i] >> dd[i];
		sumpp += pp[i];
	}
	double maxss = 0, summm = 0;
	for (int i = 0; i < mm; ++i)
	{
		cin >> ss[i];
		maxss = max(maxss, ss[i]);
		summm += ss[i];
	}

	double l = 0, r = (double) INT_MAX;
	vector<double> events(nn * 2);
	for (int IT = 0; IT < 200; ++IT)
	{
		double mid = (l + r) / 2;

		n = nn + nn * 2 - 1 + 2;

		edges.clear();
		for (int i = 0; i < n; ++i)
			a[i].clear();
		fill(d, d + n, 0);
		fill(Q, Q + n, 0);
		ql = qr = 0;
		fill(ptr, ptr + n, 0);

		events.clear();
		for (int i = 0; i < nn; ++i)
		{
			events.push_back(rr[i]);
			events.push_back(dd[i] + mid);
		}
		sort(events.begin(), events.end());
		for (int i = 0; i < nn; ++i)
			add_edge(0, i + 1, pp[i]);

		for (int i = 0; i < nn; ++i)
			for (int j = 1; j < (int) events.size(); ++j)
				if (rr[i] <= events[j - 1] && dd[i] + mid >= events[j])
					add_edge(i + 1, nn + j, (events[j] - events[j - 1]) * maxss);

		for (int j = 1; j < (int) events.size(); ++j)
			add_edge(nn + j, nn + (int) events.size(), (events[j] - events[j - 1]) * summm);

		s = 0;
		t = nn + (int) events.size();

		double ans = 0;

		while (bfs())
		{
			double pushed;
			while (abs(pushed = dfs()) > eps)
				ans += pushed;
		}

		if (abs(sumpp - ans) > eps)
			l = mid;
		else
			r = mid;
	}

	cout.precision(5);
	cout << fixed << (l + r) / 2 << "\n";

	return 0;
}
