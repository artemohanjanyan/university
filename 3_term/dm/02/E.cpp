#include <bits/stdc++.h>
using namespace std;

int const maxn = 20000;
vector<int> graph[maxn];

bool used[maxn];
int tin[maxn], tup[maxn], curTime;
int colors[maxn], color = 0;
stack<int> st;
void dfs(int v, int p = -1)
{
	st.push(v);
	used[v] = true;
	tin[v] = tup[v] = curTime++;
	for (auto to : graph[v])
		if (!used[to])
		{
			dfs(to, v);
			tup[v] = min(tup[v], tup[to]);
			if (tin[v] < tup[to])
			{
				++color;
				while (st.top() != to)
				{
					colors[st.top()] = color;
					st.pop();
				}
				colors[st.top()] = color;
				st.pop();
			}
		}
		else if (to != p)
			tup[v] = min(tup[v], tin[to]);
}

int main()
{
	ifstream cin("bicone.in");
	ofstream cout("bicone.out");

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

	for (int i = 0; i < n; ++i)
		if (!used[i])
			dfs(i);

	cout << color + 1 << "\n";
	for (int i = 0; i < n; ++i)
		cout << colors[i] + 1 << " ";

	return 0;
}
