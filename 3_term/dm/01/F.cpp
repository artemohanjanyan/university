#include <bits/stdc++.h>
using namespace std;
struct _ { ios_base::Init i; _() { cin.sync_with_stdio(0); cin.tie(0); } } _;

int const maxn = 100000; 

vector<int> graph[maxn];

int color[maxn];
int curTime;
int topsort[maxn];
bool fail = false;
void dfs(int v)
{
	color[v] = 1;
	for (auto to : graph[v])
		if (color[to] == 0)
			dfs(to);
		else if (color[to] == 1)
			fail = true;
	topsort[curTime--] = v;
	color[v] = 2;
}

int main()
{
	ifstream cin("hamiltonian.in");
	ofstream cout("hamiltonian.out");

	int n, m;
	cin >> n >> m;
	for (int i = 0; i < m; ++i)
	{
		int from, to;
		cin >> from >> to;
		--from, --to;
		graph[from].push_back(to);
	}

	curTime = n - 1;
	for (int i = 0; i < n; ++i)
		if (color[i] == 0)
			dfs(i);

	for (int i = 1; i < n; ++i)
		if (find(graph[topsort[i - 1]].begin(), graph[topsort[i - 1]].end(), topsort[i])
				== graph[topsort[i - 1]].end())
			{
				cout << "NO";
				return 0;
			}

	cout << "YES";

	return 0;
}
