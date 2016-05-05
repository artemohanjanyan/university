#include <bits/stdc++.h>
using namespace std;
struct _ { ios_base::Init i; _() { cin.sync_with_stdio(0); cin.tie(0); } } _;

int const maxn = 100000; 

vector<int> graph[maxn];
vector<int> rgraph[maxn];

int color[maxn];
int curTime;
int vert[maxn];
bool fail = false;
void topsort(int v)
{
	color[v] = 1;
	for (auto to : graph[v])
		if (color[to] == 0)
			topsort(to);
	vert[curTime--] = v;
	color[v] = 2;
}

int components[maxn];
int curComp = 0;
void dfs(int v)
{
	color[v] = 1;
	components[v] = curComp;
	for (auto to : rgraph[v])
		if (color[to] == 0)
			dfs(to);
	color[v] = 2;
}

int main()
{
	ifstream cin("cond.in");
	ofstream cout("cond.out");

	int n, m;
	cin >> n >> m;
	for (int i = 0; i < m; ++i)
	{
		int from, to;
		cin >> from >> to;
		--from, --to;
		graph[from].push_back(to);
		rgraph[to].push_back(from);
	}

	curTime = n - 1;
	for (int i = 0; i < n; ++i)
		if (color[i] == 0)
			topsort(i);

	fill(color, color + n, 0);

	for (int i = 0; i < n; ++i)
		if (color[vert[i]] == 0)
		{
			dfs(vert[i]);
			++curComp;
		}

	cout << curComp << "\n";
	for (int i = 0; i < n; ++i)
		cout << components[i] + 1 << " ";

	return 0;
}
