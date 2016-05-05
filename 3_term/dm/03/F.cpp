#include <bits/stdc++.h>
using namespace std;

using ll = long long;
int const maxn = 2000;
ll const inf = numeric_limits<int>::max();

vector<pair<int, ll>> graph[maxn];
ll d1[maxn], d2[maxn];
int updateI[maxn];
int parent[maxn];
int n;

bool used[maxn];

int main()
{
	ifstream cin("negcycle.in");
	ofstream cout("negcycle.out");

	cin >> n;
	for (int i = 0; i < n; ++i)
		for (int j = 0; j < n; ++j)
		{
			ll w;
			cin >> w;
			if (w != 1000000000)
				graph[i].push_back(make_pair(j, w));
		}

	fill(d1, d1 + n, inf);
	fill(updateI, updateI + n, -1);

	for (int i = 0; i < n; ++i)
	{
		copy(d1, d1 + n, d2);
		for (int v = 0; v < n; ++v)
			for (auto& e : graph[v])
				if (d2[e.first] > d1[v] + e.second)
				{
					d2[e.first] = d1[v] + e.second;
					updateI[e.first] = i;
					parent[e.first] = v;
				}
		swap(d1, d2);
	}

	vector<int> ans;

	int startI = -1;
	bool fl = false;
	for (int i = 0; i < n; ++i)
		if (updateI[i] == n - 1)
		{
			int v = i;
			while (!used[v])
			{
				used[v] = true;
				ans.push_back(v);
				v = parent[v];
			}
			startI = find(ans.begin(), ans.end(), v) - ans.begin();
			ans.push_back(v);
			reverse(ans.begin() + startI, ans.end());

			fl = true;
			break;
		}

	if (fl)
	{
		cout << "YES\n";
		cout << ans.size() - startI << "\n";
		for (int i = startI; i < (int) ans.size(); ++i)
			cout << ans[i] + 1 << " ";
		cout << "\n";
	}
	else
		cout << "NO\n";

	return 0;
}
