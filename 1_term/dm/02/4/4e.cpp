#include <bits/stdc++.h>
using namespace std;
typedef long long LL;
struct _ { ios_base::Init i; _() { cin.sync_with_stdio(0); cin.tie(0); } } _;

int a[40];
void gen(int left, int minv = 1, int v = 0)
{
	if (left < 0)
		return;

	if (left == 0)
	{
		for (int i = 0; i < v - 1; ++i)
			cout << a[i] << "+";
		cout << a[v - 1] << "\n";
		return;
	}

	for (int i = minv; i <= left; ++i)
	{
		a[v] = i;
		gen(left - i, i, v + 1);
	}
}

int main()
{
	//freopen("partition.in", "r", stdin);
	//freopen("partition.out", "w", stdout);

	int n;
	cin >> n;
	
	gen(n);

	return 0;
}
