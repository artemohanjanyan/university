#include <bits/stdc++.h>

int const maxn = 300001;
int const alph = 11;
int s[maxn];
int shift[maxn], ord[maxn], count[maxn];
int newShift[maxn], newOrd[maxn];

std::vector<int> stack;

void print(long long refrain)
{
	auto q = stack;
	std::cout << refrain << "\n";
	while (q.size() > 0)
	{
		std::cout << q.back() << "[" << newShift[q.back()] << "]" << " ";
		q.pop_back();
	}
	std::cout << "\n";
}

using std::cin;
using std::cout;

int main()
{
	//std::ifstream cin("refrain.in");
	//std::ofstream cout("refrain.out");

	int n, m;
	cin >> n >> m;
	for (int i = 0; i < n; ++i)
		cin >> s[i];
	s[n++] = 0;

	for (int i = 0; i < n; ++i)
		++count[s[i]];
	for (int i = 1; i < alph; ++i)
		count[i] += count[i - 1];
	for (int i = n - 1; i >= 0; --i)
		shift[--count[s[i]]] = i;
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
			lcp[n - 1] = -1;
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

	long long refrain = n - 1;
	int refrainStart = 0, refrainL = n - 1;
	stack.push_back(0);
	for (int i = 1; i < n; ++i)
	{
		while (stack.size() > 0 && lcp[stack.back()] > lcp[i])
		{
			if (stack.size() > 1)
			{
				long long upd = ((long long) lcp[stack.back()]) * (i - stack[stack.size() - 2]);
				if (refrain <= upd)
				{
					refrain = upd;
					refrainStart = shift[stack.back()];
					refrainL = lcp[stack.back()];
				}
			}
			stack.pop_back();
		}
		stack.push_back(i);
	}

	cout << refrain << "\n";
	cout << refrainL << "\n";
	for (int i = 0; i < refrainL; ++i)
		cout << s[refrainStart + i] << " ";
	cout << "\n";

	return 0;
}
