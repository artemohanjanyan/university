#include <bits/stdc++.h>
using namespace std;
typedef long long LL;
struct _ { ios_base::Init i; _() { cin.sync_with_stdio(0); cin.tie(0); } } _;

const int maxn = 10000;

int a[maxn];

bool nextCombination(int *begin, int *end, int n)
{
	int expected = n;
	for (int *i = end - 1; i >= begin; --i, --expected)
		if (*i != expected)
		{
			++(*i);
			for (int *j = i + 1; j < end; ++j)
				*j = *(j - 1) + 1;

			return true;
		}

	return false;
}

int main()
{
	ifstream cin("choose.in");
	ofstream cout("choose.out");

	int n, k;
	cin >> n >> k;

	for (int i = 0; i < k; ++i)
		a[i] = i + 1;

	for (int i = 0; i < k; ++i)
		cout << a[i] << " ";
	cout << "\n";
	while (nextCombination(a, a + k, n))
	{
		for (int i = 0; i < k; ++i)
			cout << a[i] << " ";
		cout << "\n";
	}

	return 0;
}
