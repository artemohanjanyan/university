#include <bits/stdc++.h>
using namespace std;
typedef long long LL;
struct _ { ios_base::Init i; _() { cin.sync_with_stdio(0); cin.tie(0); } } _;

const int maxn = 100000;

int a[maxn], b[maxn];

int main()
{
	//ifstream cin("lottery.in");
	//ofstream cout("lottery.out");

	int n, m;
	cin >> n >> m;

	for (int i = 0; i < m; ++i)
		cin >> a[i] >> b[i];

	double ans = double(b[m - 1]) / a[m - 1];
	for (int i = m - 2; i >= 0; --i)
		ans = 1.0 / a[i] * ((a[i + 1] - 1.0) / a[i + 1] * b[i] + ans);

	cout.precision(20);
	cout << n - ans;

	return 0;
}
