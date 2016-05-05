#include <bits/stdc++.h>
using namespace std;
struct _ { ios_base::Init i; _() { cin.sync_with_stdio(0); cin.tie(0); } } _;

const int maxn = 2000;

int dp[maxn][maxn];
int p[maxn][maxn];
string s;

void print_ans(int l, int r)
{
	if (l > r)
		return;

	if (p[l][r] == 2)
		cout << s[l];

	print_ans(l + (p[l][r] != 1), r - (p[l][r] != 0));

	if (p[l][r] == 2 && l != r)
		cout << s[r];
}

int main()
{
	freopen("palindrome.in", "r", stdin);
	freopen("palindrome.out", "w", stdout);

	cin >> s;
	int n = s.length();

	for (int i = 0; i < n; ++i)
	{
		dp[i][i] = 1;
		p[i][i] = 2;
	}

	for (int di = 1; di < n; ++di)
		for (int r = di; r < n; ++r)
		{
			int l = r - di;

			dp[l][r] = dp[l + 1][r];
			p[l][r] = 0;

			if (dp[l][r - 1] > dp[l][r])
			{
				dp[l][r] = dp[l][r - 1];
				p[l][r] = 1;
			}

			if (s[l] == s[r] && dp[l + 1][r - 1] + 2 > dp[l][r])
			{
				dp[l][r] = dp[l + 1][r - 1] + 2;
				p[l][r] = 2;
			}
		}

	cout << dp[0][n - 1] << "\n";
	print_ans(0, n - 1);

	return 0;
}
