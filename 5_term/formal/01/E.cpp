#include <bits/stdc++.h>
using namespace std;

int const MAX_N = 1000;
int const MAX_L = 1000;
long long const MOD = 1000000007;

unordered_map<char, int> graph[MAX_N];
set<int> terms;

unordered_map<bitset<100>, int> sets;
int set_count = 0;

unordered_multimap<char, int> graph1[MAX_N];
set<int> terms1;

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

void convert()
{
	queue<bitset<100>> q;
	{
		bitset<100> first;
		first[0] = true;
		sets[first] = set_count++;
		if (terms1.count(0))
			terms.insert(0);
		q.push(first);
	}

	while (!q.empty())
	{
		auto cur = q.front();
		q.pop();

		int set_id = sets[cur];

		for (char c = 'a'; c <= 'z'; ++c)
		{
			bitset<100> next;
			for (int i = 0; i < 100; ++i)
				if (cur[i] && graph1[i].count(c))
					for (auto range = graph1[i].equal_range(c); range.first != range.second; ++range.first)
						next[range.first->second] = true;

			if (next.count() == 0)
				continue;

			auto next_it = sets.find(next);
			if (next_it == sets.end())
			{
				q.push(next);
				sets[next] = set_count++;
				next_it = sets.find(next);

				for (int i = 0; i < 100; ++i)
					if (next[i] && terms1.count(i))
					{
						terms.insert(set_count - 1);
						break;
					}
			}

			graph[set_id].insert({c, next_it->second});
		}
	}
}

int main()
{
	ifstream cin("problem5.in");
	ofstream cout("problem5.out");

	int n1, m1, k1, l1;
	cin >> n1 >> m1 >> k1 >> l1;
	for (int i = 0; i < k1; ++i)
	{
		int term;
		cin >> term;
		terms1.insert(term - 1);
	}
	for (int i = 0; i < m1; ++i)
	{
		int from, to;
		char c;
		cin >> from >> to >> c;
		graph1[from - 1].insert({c, to - 1});
	}

	convert();

	for (int term : terms)
	{
		paths[term][0] = 1;
		used[term][0] = true;
	}

	dfs(0, l1);

	cout << paths[0][l1];

	return 0;
}
