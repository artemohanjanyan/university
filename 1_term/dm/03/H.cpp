#include <bits/stdc++.h>
using namespace std;
typedef long long LL;
struct _ { ios_base::Init i; _() { cin.sync_with_stdio(0); cin.tie(0); } } _;

const int maxn = 100000;

vector<pair<int, long long>> graph[maxn];

long long dp[2][maxn];

long long getAns(int v, int p = -1)
{
	for (auto to : graph[v])
		if (to.first != p)
			getAns(to.first, v);

	dp[0][v] = 0;
	for (auto to : graph[v])
		if (to.first != p)
			dp[0][v] += max(dp[0][to.first], dp[1][to.first]);

	dp[1][v] = 0;
	for (auto to : graph[v])
		if (to.first != p)
			dp[1][v] = max(dp[1][v], dp[0][v] - max(dp[0][to.first], dp[1][to.first]) + dp[0][to.first] + to.second);

	return max(dp[0][v], dp[1][v]);
}

int main()
{
	ifstream cin("matching.in");
	ofstream cout("matching.out");

	int n;
	cin >> n;
	for (int i = 1; i < n; ++i)
	{
		int from, to;
		long long weight;
		cin >> from >> to >> weight;
		--from, --to;
		graph[from].push_back(make_pair(to, weight));
		graph[to].push_back(make_pair(from, weight));
	}

	cout << getAns(0);

	return 0;
}
