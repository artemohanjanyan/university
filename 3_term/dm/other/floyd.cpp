#include <bits/stdc++.h>
using namespace std;
struct _ { ios_base::Init i; _() { cin.sync_with_stdio(0); cin.tie(0); } } _;

int const maxn = 100;

long long a[maxn][maxn];
long long b[maxn][maxn];

int main()
{
	int n = 7;
	int perm[] = {0, 1, 2, 3, 4, 5, 6};

	do
	{
		for (int i = 0; i < n; ++i)
			for (int j = 0; j < n; ++j)
				a[i][j] = b[i][j] = INT_MAX;

		for (int i = 1; i < n; ++i)
		{
			a[perm[i - 1]][perm[i]] = 1;
			a[perm[i]][perm[i - 1]] = 1;
			b[perm[i - 1]][perm[i]] = 1;
			b[perm[i]][perm[i - 1]] = 1;
		}

		for (int k = 0; k < n; ++k)
			for (int i = 0; i < n; ++i)
				for (int j = 0; j < n; ++j)
					a[i][j] = min(a[i][j], a[i][k] + a[k][j]);

		for (int ii = 0; ii < 2; ++ii)
			for (int i = 0; i < n; ++i)
				for (int j = 0; j < n; ++j)
					for (int k = 0; k < n; ++k)
						b[i][j] = min(b[i][j], b[i][k] + b[k][j]);

		for (int i = 0; i < n; ++i)
			for (int j = 0; j < n; ++j)
				if (a[i][j] != b[i][j])
				{
					for (int k = 0; k < n; ++k)
						cout << perm[k] << " ";
					cout << endl;
					cout << i << " " << j << endl;
				}
	} while (next_permutation(perm, perm + n));

	return 0;
}
