#include <bits/stdc++.h>
using namespace std;

int const MAX_N = 100000;
long long const MOD = 1000000007;

unordered_map<char, int> graph[MAX_N + 1];
vector<int> terms;

int const PROCESSING = 1;
int const PROCESSED = 2;
int const INFINITE = 4;
int const NOT_ZERO = 8;

int statuses[MAX_N + 1];
long long results[MAX_N + 1];

void dfs(int v)
{
	statuses[v] |= PROCESSING;
	for (auto const &edge : graph[v])
	{
		if (statuses[edge.second] & PROCESSING)
			statuses[edge.second] |= INFINITE;

		if (statuses[edge.second] == 0)
			dfs(edge.second);

		if (!(statuses[edge.second] & INFINITE))
			results[v] += results[edge.second];

		if (statuses[edge.second] & NOT_ZERO)
		{
			statuses[v] |= NOT_ZERO;
			if (statuses[edge.second] & INFINITE)
				statuses[v] |= INFINITE;
		}
	}
	results[v] = results[v] % MOD;
	statuses[v] ^= PROCESSING;
	statuses[v] |= PROCESSED;
}

int main()
{
	ifstream cin("problem3.in");
	ofstream cout("problem3.out");

	int n, m, k;
	cin >> n >> m >> k;
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
		graph[term]['0'] = n;
	results[n] = 1;
	statuses[n] = PROCESSED | NOT_ZERO;

	dfs(0);
	if ((statuses[0] & INFINITE) && (statuses[0] & NOT_ZERO))
		cout << "-1\n";
	else
		cout << results[0] << "\n";

	return 0;
}
