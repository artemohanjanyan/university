#include <bits/stdc++.h>
using namespace std;
typedef long long LL;
struct _ { ios_base::Init i; _() { cin.sync_with_stdio(0); cin.tie(0); } } _;

const int maxn = 10000;

int a[maxn];

int main()
{
	ifstream cin("nextchoose.in");
	ofstream cout("nextchoose.out");

	int n, k;
	cin >> n >> k;
	for (int i = 0; i < k; ++i)
		cin >> a[i];
	sort(a, a + k);

	int nn = n;
	for (int i = k - 1; i >= 0; --i)
		if (a[i] != nn--)
		{
			++a[i];
			for (int j = i + 1; j < n; ++j)
				a[j] = a[j - 1] + 1;

			for (int i = 0; i < k; ++i)
				cout << a[i] << " ";

			return 0;
		}

	cout << "-1\n";

	return 0;
}
