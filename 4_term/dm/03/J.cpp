#include <bits/stdc++.h>
using namespace std;

unsigned int const MAX_N = 30000000;
unsigned int d[MAX_N];
unsigned int tmp[MAX_N];
unsigned int sizes[1 << 16];
unsigned int indexes[1 << 16];
unsigned int mask = (1 << 16) - 1;

int main()
{
	ifstream cin("p1p1sumu.in");
	ofstream cout("p1p1sumu.out");

	int n, A, B, C, D;
	cin >> n >> d[0] >> d[1] >> A >> B >> C >> D;
	for (int i = 2; i < n; ++i)
		d[i] = (unsigned int) ((((unsigned long long) A) * d[i - 2] + ((unsigned long long) B) * d[i - 1] + C) % D);

	for (int bucket = 0; bucket < 2; ++bucket)
	{
		fill(sizes, sizes + (1 << 16), 0);

		if (bucket == 0)
			for (int i = 0; i < n; ++i)
				++sizes[d[i] & mask];
		else
			for (int i = 0; i < n; ++i)
				++sizes[(d[i] >> 16) & mask];

		indexes[0] = 0;
		for (int i = 1; i < (1 << 16); ++i)
			indexes[i] = indexes[i - 1] + sizes[i - 1];

		if (bucket == 0)
			for (int i = 0; i < n; ++i)
				tmp[indexes[d[i] & mask]++] = d[i];
		else
			for (int i = 0; i < n; ++i)
				tmp[indexes[(d[i] >> 16) & mask]++] = d[i];

		swap(d, tmp);
	}

	unsigned int ans = 0;
	for (unsigned int i = 0; i < (unsigned int) n; ++i)
		if (d[i] > ans)
			++ans;

	cout << ans;

	return 0;
}
