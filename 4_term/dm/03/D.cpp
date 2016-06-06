#include <bits/stdc++.h>
using namespace std;

using type = pair<pair<int, int>, int>;

int const MAXN = 100000;
type d[MAXN];
int ans[MAXN];

class DComp
{
public:
	bool operator()(int a, int b)
	{
		return d[a].first.second < d[b].first.second || (d[a].first.second == d[b].first.second && d[a] < d[b]);
	}
};

int main()
{
	//freopen("p1sumu.in", "r", stdin);
	//freopen("p1sumu.out", "w", stdout);

	//ios_base::sync_with_stdio(false);
	//cin.tie(NULL);

	int n;
	cin >> n;
	for (int i = 0; i < n; ++i)
	{
		cin >> d[i].first.second >> d[i].first.first;
		d[i].second = i;
	}

	sort(d, d + n);

	set<int, DComp> S;
	int t = 0;
	for (int i = 0; i < n; ++i)
	{
		S.insert(i);
		t += d[i].first.second;
		if (t > d[i].first.first)
		{
			auto it = prev(S.end());
			t -= d[*it].first.second;
			S.erase(it);
		}
	}

	fill(ans, ans + n, -1);
	int ansL = 0;
	t = 0;
	for (int i = 0; i < n; ++i)
		if (S.find(i) != S.end())
		{
			++ansL;
			ans[d[i].second] = t;
			t += d[i].first.second;
		}

	cout << ansL << "\n";
	for (int i = 0; i < n; ++i)
		cout << ans[i] << " ";

	return 0;
}
