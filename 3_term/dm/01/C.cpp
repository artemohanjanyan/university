#include <bits/stdc++.h>
using namespace std;
struct _ { ios_base::Init i; _() { cin.sync_with_stdio(0); cin.tie(0); } } _;

int const maxn = 100000;

vector<pair<int, int>> graph[maxn];

long long d[maxn];
const long long inf = numeric_limits<long long>::max() - 1000;

bool used[maxn];

void dfs(int v)
{
	used[v] = true;
	for (auto& to : graph[v])
	{
		if (!used[to.first])
			dfs(to.first);
		d[v] = min(d[v], d[to.first] + to.second);
	}
}

int main()
{
	ifstream cin("shortpath.in");
	ofstream cout("shortpath.out");

	int n, m, s, t;
	cin >> n >> m >> s >> t;
	--s, --t;
	for (int i = 0; i < m; ++i)
	{
		int from, to, w;
		cin >> from >> to >> w;
		--from, --to;
		graph[from].push_back(make_pair(to, w));
	}

	fill(d, d + n, inf);
	d[t] = 0;
	dfs(s);

	if (!used[t])
		cout << "Unreachable";
	else
		cout << d[s];

	return 0;
}
