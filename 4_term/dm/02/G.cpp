#include <bits/stdc++.h>

int const maxn = 400002;
int const alph = 'z' - 'a' + 3;
int shift[maxn], ord[maxn], count[maxn];
int newShift[maxn], newOrd[maxn];

using std::cin;
using std::cout;

int main()
{
	std::ifstream cin("common.in");
	std::ofstream cout("common.out");

	std::string s1, s2;
	cin >> s1 >> s2;
	std::string s = s1 + "`" + s2 + "_";
	int n = (int) s.size();

	for (int i = 0; i < n; ++i)
		++count[s[i] - '_'];
	for (int i = 1; i < alph; ++i)
		count[i] += count[i - 1];
	for (int i = n - 1; i >= 0; --i)
		shift[--count[s[i] - '_']] = i;
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

	int maxLength = 0, maxI = -1;
	for (int i = 0; i < (int) s.length() - 1; ++i)
		if ((shift[i] < (int) s1.length()) != (shift[i + 1] < (int) s1.length()))
			if (maxLength < lcp[i])
			{
				maxLength = lcp[i];
				maxI = shift[i];
			}

	for (int i = 0; i < maxLength; ++i)
		cout << s[maxI + i];

	return 0;
}
