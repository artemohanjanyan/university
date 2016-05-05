#include <bits/stdc++.h>
using namespace std;

int const maxn = 30000;

vector<int> graph[maxn];
queue<int> q;
bool used[maxn];
int length[maxn];

int main()
{
	ifstream cin("pathbge1.in");
	ofstream cout("pathbge1.out");

	int n, m;
	cin >> n >> m;

	for (int i = 0; i < m; ++i)
	{
		int from, to;
		cin >> from >> to;
		--from, --to;
		graph[from].push_back(to);
		graph[to].push_back(from);
	}

	q.push(0);
	length[0] = 0;
	used[0] = true;
	while (q.size() > 0)
	{
		int cur = q.front();
		q.pop();
		for (int to : graph[cur])
			if (!used[to])
			{
				q.push(to);
				used[to] = true;
				length[to] = length[cur] + 1;
			}
	}

	for (int i = 0; i < n; ++i)
		cout << length[i] << " ";

	return 0;
}
