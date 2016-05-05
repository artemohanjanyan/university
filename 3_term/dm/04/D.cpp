#include <bits/stdc++.h>
using namespace std;

int const maxn = 1000;

vector<int> graph[maxn];

bool used[maxn];
void dfs(int v)
{
	used[v] = true;
	for (auto to : graph[v])
		if (!used[to])
			dfs(to);
}

void findEulerPath(int v, ostream& out)
{
	stack<int> st;
	st.push(v);
	while (!st.empty())
	{
		int w = st.top();
		if (graph[w].size() > 0)
		{
			int to = graph[w][0];
			st.push(to);
			graph[w].erase(graph[w].begin());
			if (w != to)
				graph[to].erase(find(graph[to].begin(), graph[to].end(), w));
		}
		else
		{
			st.pop();
			out << w + 1 << " ";
		}
	}
}

int main()
{
	ifstream cin("euler.in");
	ofstream cout("euler.out");

	int n;
	int M = 0;
	cin >> n;
	for (int i = 0; i < n; ++i)
	{
		int m;
		cin >> m;
		graph[i].resize(m);
		for (int j = 0; j < m; ++j)
		{
			cin >> graph[i][j];
			--graph[i][j];
			++M;
		}
	}
	M /= 2;

	int oddCount = 0;
	int lastOdd = 0;
	for (int i = 0; i < n; ++i)
		if (graph[i].size() % 2 != 0)
		{
			++oddCount;
			lastOdd = i;
		}
	if (oddCount > 2)
	{
		cout << "NO\n";
		return 0;
	}

	for (int i = 0; i < n; ++i)
		if (graph[i].size() > 0)
		{
			dfs(i);
			break;
		}
	for (int i = 0; i < n; ++i)
		if (!used[i] && graph[i].size() > 0)
		{
			cout << "NO\n";
			return 0;
		}

	cout << M << "\n";
	findEulerPath(lastOdd, cout);

	return 0;
}
