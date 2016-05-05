#include <bits/stdc++.h>
using namespace std;
typedef long long LL;
struct _ { ios_base::Init i; _() { cin.sync_with_stdio(0); cin.tie(0); } } _;

const int maxn = 1000000, mod = 1000000007;

vector<int> a[maxn];

int ans[maxn];

int getAns(int v)
{
	if (ans[v] != -1)
		return ans[v];

	int curAns = 0;
	for (auto from : a[v])
		curAns = (curAns + getAns(from)) % mod;

	return ans[v] = curAns;
}

int main()
{
	ifstream cin("countpaths.in");
	ofstream cout("countpaths.out");

	int n, m;
	cin >> n >> m;
	for (int i = 0; i < m; ++i)
	{
		int from, to;
		cin >> from >> to;
		--from, --to;
		a[to].push_back(from);
	}

	fill(ans, ans + n, -1);
	ans[0] = 1;

	cout << getAns(n - 1);

	return 0;
}
