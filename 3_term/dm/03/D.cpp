#include <bits/stdc++.h>
using namespace std;

int const maxn = 30000;
vector<pair<int, int>> graph[maxn];
int inf = numeric_limits<int>::max();

bool used[maxn];
int length[maxn];
set<pair<int, int>> s;

int main()
{
	ifstream cin("pathbgep.in");
	ofstream cout("pathbgep.out");

	int n, m;
	cin >> n >> m;
	for (int i = 0; i < m; ++i)
	{
		int from, to, w;
		cin >> from >> to >> w;
		--from, --to;
		graph[from].push_back(make_pair(to, w));
		graph[to].push_back(make_pair(from, w));
	}

	fill(length, length + n, inf);
	length[0] = 0;
	s.insert(make_pair(0, 0));

	while (!s.empty())
	{
		auto p = *s.begin();
		s.erase(s.begin());

		int v = p.second;
		int l = p.first;

		used[v] = true;

		for (auto& to : graph[v])
			if (!used[to.first])
			{
				s.erase(make_pair(length[to.first], to.first));
				length[to.first] = min(length[to.first], l + to.second);
				s.insert(make_pair(length[to.first], to.first));
			}
	}

	for (int i = 0; i < n; ++i)
		cout << length[i] << " ";

	return 0;
}
