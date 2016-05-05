#include <bits/stdc++.h>
using namespace std;
typedef long long LL;
struct _ { ios_base::Init i; _() { cin.sync_with_stdio(0); cin.tie(0); } } _;

int fib[20];

int n;
int cur[20];
void gen(int v = 0, int ones = 0)
{
	if (v == n)
	{
		for (int i = 0; i < n; ++i)
			cout << cur[i];
		cout << "\n";
	}
	else
	{
		cur[v] = 0;
		gen(v + 1, 0);

		if (ones < 1)
		{
			cur[v] = 1;
			gen(v + 1, ones + 1);
		}
	}
}

int main()
{
	freopen("vectors.in", "r", stdin);
	freopen("vectors.out", "w", stdout);

	fib[0] = 0;
	fib[1] = 1;
	for (int i = 2; i < 20; ++i)
		fib[i] = fib[i - 1] + fib[i - 2];

	cin >> n;

	cout << fib[n + 2] << "\n";
	gen();

	return 0;
}
