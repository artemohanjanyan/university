#include <bits/stdc++.h>

int const maxn = 400001;
int const alph = 'z' - 'a' + 2;
int shift[maxn], ord[maxn], count[maxn];
int newShift[maxn], newOrd[maxn];

using std::cin;
using std::cout;

int main()
{
	std::ifstream cin("count.in");
	std::ofstream cout("count.out");

	std::string s;
	s.reserve(maxn);
	cin >> s;
	s.append("`");
	int n = (int) s.size();

	for (int i = 0; i < n; ++i)
		++count[s[i] - '`'];
	for (int i = 1; i < alph; ++i)
		count[i] += count[i - 1];
	for (int i = n - 1; i >= 0; --i)
		shift[--count[s[i] - '`']] = i;
	int ordN = 1;
	ord[shift[0]] = 0;
	for (int i = 1; i < n; ++i)
	{
		if (s[shift[i - 1]] != s[shift[i]])
			++ordN;
		ord[shift[i]] = ordN - 1;
	}

	for (int stepI = 0; (1 << stepI) < n; ++stepI)
	{
		for (int i = 0; i < n; ++i)
			shift[i] = (shift[i] - (1 << stepI) + n) % n;

		std::fill(count, count + n, 0);
		for (int i = 0; i < n; ++i)
			++count[ord[shift[i]]];
		for (int i = 1; i < n; ++i)
			count[i] += count[i - 1];
		for (int i = n - 1; i >= 0; --i)
			newShift[--count[ord[shift[i]]]] = shift[i];

		ordN = 1;
		newOrd[newShift[0]] = 0;
		for (int i = 1; i < n; ++i)
		{
			if (ord[newShift[i]] != ord[newShift[i - 1]] ||
					ord[(newShift[i] + (1 << stepI)) % n] != ord[(newShift[i - 1] + (1 << stepI)) % n])
				++ordN;
			newOrd[newShift[i]] = ordN - 1;
		}
		
		std::swap(shift, newShift);
		std::swap(ord, newOrd);
	}

	int *lcp = newShift, *revShift = newOrd;
	for (int i = 0; i < n; ++i)
		revShift[shift[i]] = i;
	for (int k = 0, i = 0; i < n; ++i)
	{
		if (k > 0)
			--k;
		if (revShift[i] == n - 1)
		{
			lcp[n - 1] = 0;
			k = 0;
		}
		else
		{
			int j = shift[revShift[i] + 1];
			while (std::max(i + k, j + k) < n && s[i + k] == s[j + k])
				++k;
			lcp[revShift[i]] = k;
		}
	}

	long long ans = ((long long) n) * (n - 1) / 2;
	for (int i = 0; i < n; ++i)
		ans -= lcp[i];

	cout << ans;

	return 0;
}
