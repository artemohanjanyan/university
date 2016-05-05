#include <bits/stdc++.h>
using namespace std;
typedef long long LL;
struct _ { ios_base::Init i; _() { cin.sync_with_stdio(0); cin.tie(0); } } _;

const int maxn = 50;

LL d[maxn][maxn];

int main()
{
	//ifstream cin("num2brackets2.in");
	//ofstream cout("num2brackets2.out");

	LL n, k;
	cin >> n >> k;
	n *= 2;

	d[0][0] = 1;
	for (int i = 1; i <= n; ++i)
		for (int j = 0; j <= n; ++j)
		{
			if (j > 0)
				d[i][j] += d[i-1][j-1];
			d[i][j] += d[i-1][j+1];
		}

	//for (int i = 0; i <= n; ++i)
	//{
	//	for (int j = 0; j <= n; ++j)
	//		cerr << d[i][j] << " ";
	//	cerr << endl;
	//}
	
	stack<char> notClosed;
	string ans;

	int balance = 0;
	while (n > 0)
	{
		if (k < d[n-1][balance+1] * (1 << ((n - 1 - balance - 1) / 2)))
		{
			ans += '(';
			notClosed.push('(');
			++balance;
			--n;

			continue;
		}
		k -= d[n-1][balance+1] * (1 << ((n - 1 - balance - 1) / 2)); 

		if (!notClosed.empty() && notClosed.top() == '(')
		{
			if (k < d[n-1][balance-1] * (1 << ((n - 1 - balance + 1) / 2)))
			{
				ans += ')';
				notClosed.pop();
				--n;
				--balance;

				continue;
			}
			k -= d[n-1][balance-1] * (1 << ((n - 1 - balance + 1) / 2));
		}

		if (k < d[n-1][balance+1] * (1 << ((n - 1 - balance - 1) / 2)))
		{
			ans += '[';
			notClosed.push('[');
			++balance;
			--n;

			continue;
		}
		k -= d[n-1][balance+1] * (1 << ((n - 1 - balance - 1) / 2)); 

		if (!notClosed.empty() && notClosed.top() == '[')
		{
			if (k < d[n-1][balance-1] * (1 << ((n - 1 - balance + 1) / 2)))
			{
				ans += ']';
				notClosed.pop();
				--n;
				--balance;

				continue;
			}
			k -= d[n-1][balance-1] * (1 << ((n - 1 - balance + 1) / 2));
		}
	}

	cout << ans << endl;

	return 0;
}
