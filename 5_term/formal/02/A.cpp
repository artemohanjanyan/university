#include <bits/stdc++.h>
using namespace std;

int const MAX_N = 100000;

map<char, int> graph[MAX_N];
bool is_term[MAX_N];
bool used[MAX_N];
map<char, int> graph2[MAX_N];
bool is_term2[MAX_N];
bool used2[MAX_N];

void compare(int v, int v2)
{
	used[v] = true;
	used2[v2] = true;
	if (is_term[v] != is_term2[v2])
	{
		cout << "NO\n";
		exit(0);
	}

	for (char c = 'a'; c <= 'z'; ++c)
	{
		auto cc = graph[v].count(c);
		if (cc != graph2[v2].count(c) || (cc && used[graph[v][c]] != used2[graph2[v2][c]]))
		{
			cout << "NO\n";
			exit(0);
		}
		if (cc && !used[graph[v][c]])
			compare(graph[v][c], graph2[v2][c]);
	}
}

int main()
{
	freopen("isomorphism.in", "r", stdin);
	freopen("isomorphism.out", "w", stdout);

	int n, m, k;
	cin >> n >> m >> k;
	for (int i = 0; i < k; ++i)
	{
		int v;
		cin >> v;
		is_term[v - 1] = true;
	}
	for (int i = 0; i < m; ++i)
	{
		int from, to;
		char c;
		cin >> from >> to >> c;
		graph[from - 1][c] = to - 1;
	}

	int n2, m2, k2;
	cin >> n2 >> m2 >> k2;
	for (int i = 0; i < k2; ++i)
	{
		int v;
		cin >> v;
		is_term2[v - 1] = true;
	}
	for (int i = 0; i < m2; ++i)
	{
		int from, to;
		char c;
		cin >> from >> to >> c;
		graph2[from - 1][c] = to - 1;
	}

	if (n != n2 || m != m2 || k != k2)
	{
		cout << "NO\n";
		return 0;
	}

	compare(0, 0);

	cout << "YES\n";

	return 0;
}
