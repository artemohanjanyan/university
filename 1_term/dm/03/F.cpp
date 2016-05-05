#include <bits/stdc++.h>
using namespace std;
typedef long long LL;
struct _ { ios_base::Init i; _() { cin.sync_with_stdio(0); cin.tie(0); } } _;

const int maxn = 400;

long long dp[maxn][maxn];
int p[maxn][maxn];

pair<int, int> matrixes[maxn];

void print_ans(int l, int r)
{
	if (l == r)
		cout << 'A';
	else
	{
		cout << '(';
		print_ans(l, p[l][r]);
		print_ans(p[l][r] + 1, r);
		cout << ')';
	}
}

int main()
{
	freopen("matrix.in", "r", stdin);
	freopen("matrix.out", "w", stdout);

	int n;
	cin >> n;
	for (int i = 0; i < n; ++i)
		cin >> matrixes[i].first >> matrixes[i].second;

	for (int di = 1; di < n; ++di)
		for (int r = di; r < n; ++r)
		{
			int l = r - di;
			dp[l][r] = INT_MAX;

			for (int mid = l; mid < r; ++mid)
			{
				long long newCost = dp[l][mid] + dp[mid + 1][r] + matrixes[l].first * matrixes[mid].second * matrixes[r].second;
				if (newCost < dp[l][r])
				{
					dp[l][r] = newCost;
					p[l][r] = mid;
				}
			}
		}

	print_ans(0, n - 1);

	return 0;
}
