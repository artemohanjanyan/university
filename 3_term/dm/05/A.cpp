#include <bits/stdc++.h>
using namespace std;
struct _ { ios_base::Init i; _() { cin.sync_with_stdio(0); cin.tie(0); } } _;

int const maxn = 200;

vector<int> graph[maxn];
int left_p[maxn];

int colors[maxn], color;
bool dfs(int v)
{
	colors[v] = color;
	for (int i = 0; i < (int) graph[v].size(); ++i)
		if (left_p[graph[v][i]] == -1 ||
				(colors[left_p[graph[v][i]]] != color && dfs(left_p[graph[v][i]])))
		{
			left_p[graph[v][i]] = v;
			return true;
		}
	return false;
}

int main()
{
	ifstream cin("matching.in");
	ofstream cout("matching.out");

	int n, m, k;
	cin >> n >> m >> k;

	for (int i = 0; i < k; ++i)
	{
		int a, b;
		cin >> a >> b;
		--a, --b;
		graph[a].push_back(b);
	}

	fill(left_p, left_p + m, -1);
	for (int i = 0; i < n; ++i)
	{
		++color;
		dfs(i);
	}

	cout << m - count(left_p, left_p + m, -1) << endl;

	return 0;
}
