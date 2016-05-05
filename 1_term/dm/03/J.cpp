#include <bits/stdc++.h>
using namespace std;
typedef long long LL;
struct _ { ios_base::Init i; _() { cin.sync_with_stdio(0); cin.tie(0); } } _;

const int maxn = 2000;

long long sum[maxn][maxn], dp[maxn][maxn], parent[maxn][maxn], p[maxn];

char codeStack[maxn + 1];
void print_ans(int l, int r, int depth)
{
	if (l == r)
	{
		codeStack[depth] = '\0';
		cout << codeStack << "\n";
		return;
	}

	codeStack[depth] = '0';
	print_ans(l, parent[l][r], depth + 1);

	codeStack[depth] = '1';
	print_ans(parent[l][r] + 1, r, depth + 1);
}

int main()
{
	freopen("optimalcode.in", "r", stdin);
	freopen("optimalcode.out", "w", stdout);

	int n;
	cin >> n;
	for (int i = 0; i < n; ++i)
		cin >> p[i];

	for (int i = 0; i < n; ++i)
		sum[i][i] = p[i];
	for (int di = 1; di < n; ++di)
		for (int r = di; r < n; ++r)
			sum[r - di][r] = sum[r - di + 1][r] + p[r - di];

	for (int i = 0; i < n; ++i)
		parent[i][i] = i;

	for (int di = 1; di < n; ++di)
		for (int r = di; r < n; ++r)
		{
			int l = r - di;

			dp[l][r] = LLONG_MAX;
			for (int mid = parent[l][r - 1]; mid <= min(parent[l + 1][r], (long long) r - 1); ++mid)
				if (dp[l][r] >= dp[l][mid] + dp[mid + 1][r])
				{
					dp[l][r] = dp[l][mid] + dp[mid + 1][r];
					parent[l][r] = mid;
				}

			dp[l][r] += sum[l][r];
		}

	cout << dp[0][n - 1] << "\n";
	print_ans(0, n - 1, 0);

	return 0;
}
