#include <bits/stdc++.h>
using namespace std;
typedef long long LL;
struct _ { ios_base::Init i; _() { cin.sync_with_stdio(0); cin.tie(0); } } _;

int a[10], n;

void gen(int v = 0, int minv = 1)
{
	for (int i = 0; i < v; ++i)
		cout << a[i] << " ";
	cout << endl;

	if (v == n)
		return;

	for (int i = minv; i <= n; ++i)
	{
		a[v] = i;
		gen(v + 1, i + 1);
	}
}

int main()
{
	freopen("subsets.in", "r", stdin);
	freopen("subsets.out", "w", stdout);

	cin >> n;

	gen();

	return 0;
}
