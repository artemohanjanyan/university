#include <bits/stdc++.h>
using namespace std;
typedef long long LL;

int main()
{
	freopen("exam.in", "r", stdin);
	freopen("exam.out", "w", stdout);

	int n, k;
	scanf("%d%d", &k, &n);
	int sum = 0, p, m;
	for (int i = 0; i < k; ++i)
	{
		scanf("%d%d", &p, &m);
		sum += p * m;
	}

	printf("%lf\n", double(sum) / n / 100);

	return 0;
}
