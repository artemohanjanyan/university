#include <bits/stdc++.h>
using namespace std;

int const maxn = 20000, maxm = 200000;
vector<pair<int, int>> graph[maxn];

bool used[maxn];
int tin[maxn], tup[maxn], curTime;

int colors[maxm], color;
stack<int> st;
int edgeUsed[maxm];

void push_edge(int i)
{
	if (!edgeUsed[i])
	{
		st.push(i);
		edgeUsed[i] = true;
	}
}

void dfs(int v, int p = -1)
{
	used[v] = true;
	tin[v] = tup[v] = curTime++;
	for (auto to : graph[v])
		if (!used[to.first])
		{
			push_edge(to.second);
			dfs(to.first, v);
			tup[v] = min(tup[v], tup[to.first]);
			if (p != -1 && tin[v] <= tup[to.first] || p == -1)
			{
				while (st.top() != to.second)
				{
					colors[st.top()] = color;
					st.pop();
				}
				colors[st.top()] = color;
				st.pop();
				++color;
			}
		}
		else if (to.first != p)
		{
			tup[v] = min(tup[v], tin[to.first]);
			push_edge(to.second);
		}
}

int main()
{
	ifstream cin("biconv.in");
	ofstream cout("biconv.out");

	int n, m;
	cin >> n >> m;
	for (int i = 0; i < m; ++i)
	{
		int from, to;
		cin >> from >> to;
		--from, --to;
		graph[from].push_back(make_pair(to, i));
		graph[to].push_back(make_pair(from, i));
	}

	for (int i = 0; i < n; ++i)
		if (!used[i])
			dfs(i);

	cout << color << "\n";
	for (int i = 0; i < m; ++i)
		cout << colors[i] + 1 << " ";

	return 0;
}
