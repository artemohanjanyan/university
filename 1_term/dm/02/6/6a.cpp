#include <bits/stdc++.h>
using namespace std;
typedef long long LL;
struct _ { ios_base::Init i; _() { cin.sync_with_stdio(0); cin.tie(0); } } _;

int main()
{
	ifstream cin("nextvector.in");
	ofstream cout("nextvector.out");

	string s;
	cin >> s;

	int n = s.size();

	bool fl = true;
	for (int i = 0; i < n; ++i)
		fl &= (s[i] == '0');

	if (fl)
		cout << "-\n";
	else
	{
		string s1 = s;
		for (int i = n - 1; i >= 0; --i)
			if (s1[i] == '1')
			{
				s1[i] = '0';
				for (int j = i + 1; j < n; ++j)
					s1[j] = '1';
				break;
			}
		cout << s1 << "\n";
	}

	fl = true;
	for (int i = 0; i < n; ++i)
		fl &= (s[i] == '1');

	if (fl)
		cout << "-\n";
	else
	{
		string s1 = s;
		for (int i = n - 1; i >= 0; --i)
			if (s1[i] == '0')
			{
				s1[i] = '1';
				for (int j = i + 1; j < n; ++j)
					s1[j] = '0';
				break;
			}
		cout << s1 << "\n";
	}

	return 0;
}
