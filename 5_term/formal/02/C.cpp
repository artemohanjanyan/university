#include <bits/stdc++.h>
using namespace std;

int const MAX_N = 1001;
int const ALPH = 26;

int n;
int graph[MAX_N][ALPH];
bool is_term[MAX_N];

unordered_map<int, vector<int>> reverse_graph[MAX_N];
void build_reverse()
{
	for (int i = 0; i <= n; ++i)
		for (int c = 0; c < ALPH; ++c)
			reverse_graph[graph[i][c]][c].push_back(i);
}

bool reachable[MAX_N];
void find_reachable(int v = 1)
{
	reachable[v] = true;
	for (int c = 0; c < ALPH; ++c)
		if (!reachable[graph[v][c]])
			find_reachable(graph[v][c]);
}

bool non_equivalent[MAX_N][MAX_N];
void build_table()
{
	queue<pair<int, int>> q;
	for (int i = 0; i <= n; ++i)
		for (int j = 0; j <= n; ++j)
			if (!non_equivalent[i][j] && is_term[i] != is_term[j])
			{
				non_equivalent[i][j] = non_equivalent[j][i] = true;
				q.push({i, j});
			}

	while (!q.empty())
	{
		auto next = q.front();
		q.pop();
		for (int c = 0; c < ALPH; ++c)
			for (int prev_first : reverse_graph[next.first][c])
				for (int prev_second : reverse_graph[next.second][c])
					if (!non_equivalent[prev_first][prev_second])
					{
						non_equivalent[prev_first][prev_second] = non_equivalent[prev_second][prev_first] = true;
						q.push({prev_first, prev_second});
					}
	}
}

int minimal_n;
int minimal_graph[MAX_N][ALPH];
bool minimal_is_term[MAX_N];
int components[MAX_N];
void build_minimal_graph()
{
	fill(components, components + n + 1, -1);
	for (int i = 0; i <= n; ++i)
		if (!non_equivalent[0][i])
			components[i] = 0;

	minimal_n = 0;
	for (int i = 1; i <= n; ++i)
	{
		if (!reachable[i] || components[i] != -1)
			continue;
		
		components[i] = ++minimal_n;
		for (int j = i + 1; j <= n; ++j)
			if (!non_equivalent[i][j])
				components[j] = minimal_n;
	}

	for (int i = 1; i <= n; ++i)
		if (reachable[i] && components[i] != 0)
		{
			minimal_is_term[components[i]] = is_term[i];
			for (int c = 0; c < ALPH; ++c)
				minimal_graph[components[i]][c] = components[graph[i][c]];
		}
}

int main()
{
	ifstream cin("minimization.in");
	ofstream cout("minimization.out");

	int m, k;
	cin >> n >> m >> k;
	for (int i = 0; i < k; ++i)
	{
		int v;
		cin >> v;
		is_term[v] = true;
	}

	for (int i = 0; i < m; ++i)
	{
		int from, to;
		char c;
		cin >> from >> to >> c;
		graph[from][static_cast<int>(c - 'a')] = to;
	}

	build_reverse();
	find_reachable();
	build_table();
	build_minimal_graph();

	int minimal_m = 0, minimal_k = 0;
	for (int i = 1; i <= minimal_n; ++i)
	{
		if (minimal_is_term[i])
			++minimal_k;
		for (int c = 0; c < ALPH; ++c)
			if (minimal_graph[i][c] != 0)
				++minimal_m;
	}
	cout << minimal_n << " " << minimal_m << " " << minimal_k << "\n";
	for (int i = 1; i <= minimal_n; ++i)
		if (minimal_is_term[i])
			cout << i << " ";
	cout << "\n";
	for (int i = 1; i <= minimal_n; ++i)
		for (int c = 0; c < ALPH; ++c)
			if (minimal_graph[i][c] != 0)
				cout << i << " " << minimal_graph[i][c] << " " << static_cast<char>(c + 'a') << "\n";

	return 0;
}
