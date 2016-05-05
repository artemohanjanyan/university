#include <bits/stdc++.h>
using namespace std;
struct _ { ios_base::Init i; _() { cin.sync_with_stdio(0); cin.tie(0); } } _;

int const maxn = 300;

int n, cur_min;
int graph[maxn][maxn];
int left_p[maxn];

int colors[maxn], color;
bool dfs(int v)
{
	colors[v] = color;
	for (int i = 0; i < n; ++i) if (graph[v][i] >= cur_min)
		if (left_p[i] == -1 ||
				(colors[left_p[i]] != color && dfs(left_p[i])))
		{
			left_p[i] = v;
			return true;
		}
	return false;
}

int main()
{
	ifstream cin("minimax.in");
	ofstream cout("minimax.out");

	cin >> n;

	for (int i = 0; i < n; ++i)
		for (int j = 0; j < n; ++j)
			cin >> graph[i][j];

	int l = 0, r = 1000001;

	while (l + 1 < r)
	{
		cur_min = l + (r - l) / 2;

		fill(left_p, left_p + n, -1);
		for (int i = 0; i < n; ++i)
		{
			++color;
			dfs(i);
		}

		if (count(left_p, left_p + n, -1) == 0)
			l = cur_min;
		else
			r = cur_min;
	}

	cout << l << endl;

	return 0;
}
