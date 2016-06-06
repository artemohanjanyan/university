#include <bits/stdc++.h>
using namespace std;

using type = pair<pair<int, int>, int>;

int const MAXN = 200000;
type d[MAXN];
int ans[MAXN];

class WComp
{
public:
	bool operator()(int a, int b)
	{
		return d[a].first.second < d[b].first.second || d[a].first.second == d[b].first.second && d[a] < d[b];
	}
};

int main()
{
	freopen("p1sumwu.in", "r", stdin);
	freopen("p1sumwu.out", "w", stdout);

	ios_base::sync_with_stdio(false);
	cin.tie(NULL);

	int n;
	cin >> n;
	for (int i = 0; i < n; ++i)
	{
		cin >> d[i].first.first >> d[i].first.second;
		d[i].second = i;
	}

	sort(d, d + n);

	set<int, WComp> S;
	int t = 1;
	for (int i = 0; i < n; ++i)
	{
		S.insert(i);
		if (d[i].first.first >= t)
			++t;
		else
			S.erase(S.begin());
	}

	fill(ans, ans + n, -1);
	t = 0;
	for (int i = 0; i < n; ++i)
		if (S.find(i) != S.end())
			ans[d[i].second] = t++;
	long long fail = 0;
	for (int i = 0; i < n; ++i)
		if (S.find(i) == S.end())
		{
			ans[d[i].second] = t++;
			fail += d[i].first.second;
		}

	cout << fail << "\n";
	for (int i = 0; i < n; ++i)
		cout << ans[i] << " ";

	return 0;
}
