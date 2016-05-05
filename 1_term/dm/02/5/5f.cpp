#include <bits/stdc++.h>
using namespace std;
typedef long long LL;
struct _ { ios_base::Init i; _() { cin.sync_with_stdio(0); cin.tie(0); } } _;

const int maxn = 50;

LL d[maxn][maxn];

int main()
{
	ifstream cin("brackets2num.in");
	ofstream cout("brackets2num.out");

	string s;
	cin >> s;

	int n = s.length();

	d[0][0] = 1;
	for (int i = 1; i <= n; ++i)
		for (int j = 0; j <= n; ++j)
		{
			if (j > 0)
				d[i][j] += d[i-1][j-1];
			d[i][j] += d[i-1][j+1];
		}

	LL k = 0;
	int balance = 0;
	for (int i = 0; i < n; ++i)
		if (s[i] == '(')
			++balance;
		else
		{
			--balance;
			k += d[n-i-1][balance+2];
		}

	cout << k << endl;

	return 0;
}
