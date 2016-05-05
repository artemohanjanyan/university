#include <bits/stdc++.h>
using namespace std;

int main()
{
	ifstream cin("search2.in");
	ofstream cout("search2.out");

	string s1, s2;
	cin >> s1 >> s2;

	string s = s1 + '#' + s2;

	vector<int> z(s.length());

	z[0] = 0;

	for (int i = 1, l = 0, r = 0; i < (int) s.length(); ++i)
	{
		if (i <= r)
			z[i] = min(z[i - l], r - i + 1);
		while (i + z[i] < (int) s.length() && s[i + z[i]] == s[z[i]])
			++z[i];
		if (i + z[i] > r)
		{
			l = i;
			r = i + z[i] - 1;
		}
	}

	vector<int> ans;
	for (int i = 1 + (int) s1.length(); i < (int) s.size(); ++i)
		if (z[i] == (int) s1.length())
			ans.push_back(i - (int) s1.length());

	cout << ans.size() << "\n";
	for (int i : ans)
		cout << i << " ";

	return 0;
}
