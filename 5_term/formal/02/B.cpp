#include <bits/stdc++.h>
using namespace std;

int const MAX_N = 100001;

map<char, int> graph[MAX_N];
bool is_term[MAX_N];
bool used[MAX_N + 1];
map<char, int> graph2[MAX_N];
bool is_term2[MAX_N + 1];
bool used2[MAX_N];

bool compare()
{
	queue<pair<int, int>> q;

	q.push({0, 0});

	while (!q.empty())
	{
		auto cur = q.front();
		q.pop();
		if (is_term[cur.first] != is_term2[cur.second])
			return false;
		used[cur.first] = used2[cur.second] = true;
		for (char c = 'a'; c <= 'z'; ++c)
		{
			int to = graph[cur.first].count(c) ? graph[cur.first][c] : MAX_N - 1;
			int to2 = graph2[cur.second].count(c) ? graph2[cur.second][c] : MAX_N - 1;

			if (!used[to] || !used2[to2])
				q.push({to, to2});
		}
	}
	return true;
}

int main()
{
	ifstream cin("equivalence.in");
	ofstream cout("equivalence.out");

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

	cout << (compare() ? "YES\n" : "NO\n");

	return 0;
}
