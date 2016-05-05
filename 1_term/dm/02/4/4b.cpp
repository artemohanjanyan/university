#include <bits/stdc++.h>
using namespace std;
typedef long long LL;
struct _ { ios_base::Init i; _() { cin.sync_with_stdio(0); cin.tie(0); } } _;

int a[8] = {1, 2, 3, 4, 5, 6, 7, 8};

bool nextPermutation(int *begin, int *end)
{
	int *i = end;
	while (begin < --i)
		if (*(i - 1) < *i)
		{
			int *j = i;
			--i;
			while (j < end && *i < *j)
				++j;
			--j;

			swap(*i, *j);

			for (++i, j = end - 1; i < j; ++i, --j)
				swap(*i, *j);

			return true;
		}

	return false;
}

int main()
{
	ifstream cin("permutations.in");
	ofstream cout("permutations.out");

	int n;
	cin >> n;

	for (int i = 0; i < n; ++i)
		cout << a[i] << " ";
	cout << "\n";
	while (nextPermutation(a, a + n))
	{
		for (int i = 0; i < n; ++i)
			cout << a[i] << " ";
		cout << "\n";
	}
	
	return 0;
}
