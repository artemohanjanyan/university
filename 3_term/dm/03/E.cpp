#include <bits/stdc++.h>
using namespace std;

using ll = long long;
int const maxn = 2000;
ll const inf = numeric_limits<ll>::max();

vector<pair<int, ll>> graph[maxn];
ll d1[maxn], d2[maxn];
int updateI[maxn];
int n;

bool used[maxn];
void dfs(int v)
{
	used[v] = true;
	updateI[v] = n - 1;
	for (auto& e : graph[v])
		if (!used[e.first])
			dfs(e.first);
}

int main()
{
	ifstream cin("path.in");
	ofstream cout("path.out");

	int m, s;
	cin >> n >> m >> s;
	--s;
	for (int i = 0; i < m; ++i)
	{
		int from, to;
		ll w;
		cin >> from >> to >> w;
		--from, --to;
		graph[from].push_back(make_pair(to, w));
	}

	fill(d1, d1 + n, inf);
	d1[s] = 0;
	fill(updateI, updateI + n, -1);

	//cerr << endl;
	for (int i = 0; i < n; ++i)
	{
		copy(d1, d1 + n, d2);
		for (int v = 0; v < n; ++v)
			for (auto& e : graph[v])
				if (d1[v] != inf && d2[e.first] > d1[v] + e.second)
				{
					d2[e.first] = d1[v] + e.second;
					updateI[e.first] = i;
				}
		swap(d1, d2);
		//for (int i = 0; i < n; ++i)
		//	cerr << d1[i] << " ";
		//cerr << endl;
	}
	//cerr << endl;

	for (int i = 0; i < n; ++i)
		if (updateI[i] == n - 1 && !used[i])
			dfs(i);

	for (int i = 0; i < n; ++i)
	{
		if (updateI[i] == n - 1)
			cout << '-';
		else if (d1[i] == inf)
			cout << '*';
		else
			cout << d1[i];
		cout << "\n";
	}

	return 0;
}
