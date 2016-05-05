#include <bits/stdc++.h>
using namespace std;
typedef long long LL;
struct _ { ios_base::Init i; _() { cin.sync_with_stdio(0); cin.tie(0); } } _;

const int maxn = 5000;

int dp[maxn + 1][maxn + 1];

int main()
{
	cerr << dp << endl;

	ifstream cin("levenshtein.in");
	ofstream cout("levenshtein.out");

	string s1, s2;
	cin >> s1 >> s2;

	for (int i = 0; i <= s1.size(); ++i)
		dp[i][0] = i;
	for (int j = 0; j <= s2.size(); ++j)
		dp[0][j] = j;

	for (int i = 1; i <= s1.size(); ++i)
		for (int j = 1; j <= s2.size(); ++j)
			dp[i][j] = min(min(dp[i - 1][j], dp[i][j - 1]) + 1, dp[i - 1][j - 1] + (s1[i - 1] != s2[j - 1]));

	cout << dp[s1.size()][s2.size()] << "\n";

	return 0;
}
