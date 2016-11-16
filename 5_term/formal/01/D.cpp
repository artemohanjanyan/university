#include <bits/stdc++.h>
using namespace std;

int const MAX_N = 100;
int const MAX_L = 1000;
long long const MOD = 1000000007;

unordered_map<char, int> graph[MAX_N];
vector<int> terms;

long long paths[MAX_N][MAX_L + 1];
bool used[MAX_N][MAX_L + 1];

void dfs(int v, int length)
{
	used[v][length] = true;
	if (length == 0)
		return;

	for (auto const &edge : graph[v])
	{
		if (!used[edge.second][length - 1])
			dfs(edge.second, length - 1);
		paths[v][length] += paths[edge.second][length - 1];
	}

	paths[v][length] %= MOD;
}

int main()
{
	ifstream cin("problem4.in");
	ofstream cout("problem4.out");

	int n, m, k, l;
	cin >> n >> m >> k >> l;
	terms.resize(k);
	for (int &term : terms)
		cin >> term, --term;
	for (int i = 0; i < m; ++i)
	{
		int from, to;
		char c;
		cin >> from >> to >> c;
		graph[from - 1][c] = to - 1;
	}
	for (int term : terms)
	{
		paths[term][0] = 1;
		used[term][0] = true;
	}

	dfs(0, l);

	cout << paths[0][l];

	return 0;
}
