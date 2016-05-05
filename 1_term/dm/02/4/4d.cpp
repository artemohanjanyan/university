#include <bits/stdc++.h>
using namespace std;
typedef long long LL;
struct _ { ios_base::Init i; _() { cin.sync_with_stdio(0); cin.tie(0); } } _;

bool nextSequence(string &s, int n)
{
	int balance = 0;

	for (int i = n - 1; i >= 0; --i)
	{
		if (s[i] == '(')
			--balance;
		else
			++balance;

		if (s[i] == '(' && balance > 0)
		{
			s[i] = ')';
			--balance;

			int left = n - i - 1;
			for (int j = 0; j < (left - balance) / 2; ++j)
				s[i + j + 1] = '(';
			balance += (left - balance) / 2;
			for (int j = n - balance; j < n; ++j)
				s[j] = ')';

			return true;
		}
	}

	return false;
}

int main()
{
	ifstream cin("brackets.in");
	ofstream cout("brackets.out");

	int n;
	cin >> n;

	string s(2 * n, '(');
	for (int i = n; i < 2 * n; ++i)
		s[i] = ')';
	n *= 2;

	cout << s << "\n";
	while (nextSequence(s, n))
		cout << s << "\n";

	return 0;
}
