#include <bits/stdc++.h>
using namespace std;

int const maxn = 200;
int const inf = numeric_limits<int>::max() / 2;

int graph[maxn][maxn];

int main()
{
	ifstream cin("pathsg.in");
	ofstream cout("pathsg.out");

	int n, m;
	cin >> n >> m;

	for (int i = 0; i < n; ++i)
		for (int j = 0; j < n; ++j)
			graph[i][j] = inf * (i != j);

	for (int i = 0; i < m; ++i)
	{
		int from, to, w;
		cin >> from >> to >> w;
		--from, --to;
		graph[from][to] = w;
	}

	for (int k = 0; k < n; ++k)
		for (int i = 0; i < n; ++i)
			for (int j = 0; j < n; ++j)
				graph[i][j] = min(graph[i][j], graph[i][k] + graph[k][j]);

	for (int i = 0; i < n; ++i)
	{
		for (int j = 0; j < n; ++j)
			cout << graph[i][j] << " ";
		cout << "\n";
	}

	return 0;
}
