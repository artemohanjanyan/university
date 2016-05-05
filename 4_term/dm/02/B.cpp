#include <bits/stdc++.h>

int const maxn = 400001;
int const alph = 'z' - '`' + 1;
int shift[maxn], ord[maxn], count[maxn];
int newShift[maxn], newOrd[maxn];

int main()
{
	std::ifstream cin("array.in");
	std::ofstream cout("array.out");

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

	for (int i = 1; i < n; ++i)
		cout << shift[i] + 1 << " ";
	cout << std::endl;

	return 0;
}
