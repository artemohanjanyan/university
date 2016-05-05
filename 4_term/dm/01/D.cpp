#include <bits/stdc++.h>
using namespace std;

int main()
{
	ifstream cin("z.in");
	ofstream cout("z.out");

	string s;
	cin >> s;

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

	for (int i = 1; i < (int) z.size(); ++i)
		cout << z[i] << " ";

	return 0;
}
