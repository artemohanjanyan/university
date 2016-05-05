#include <bits/stdc++.h>
using namespace std;

int main()
{
	ifstream cin("search3.in");
	ofstream cout("search3.out");

	string p, t;
	cin >> p >> t;

	string s1 = p + "#" + t;
	vector<int> z1(s1.length());
	z1[0] = 0;
	for (int i = 1, l = 0, r = 0; i < (int) s1.length(); ++i)
	{
		if (i <= r)
			z1[i] = min(z1[i - l], r - i + 1);
		while (i + z1[i] < (int) s1.length() && s1[i + z1[i]] == s1[z1[i]])
			++z1[i];
		if (i + z1[i] > r)
		{
			l = i;
			r = i + z1[i] - 1;
		}
	}

	reverse(p.begin(), p.end());
	reverse(t.begin(), t.end());

	string s2 = p + "#" + t;
	vector<int> z2(s2.length());
	z2[0] = 0;
	for (int i = 1, l = 0, r = 0; i < (int) s2.length(); ++i)
	{
		if (i <= r)
			z2[i] = min(z2[i - l], r - i + 1);
		while (i + z2[i] < (int) s2.length() && s2[i + z2[i]] == s2[z2[i]])
			++z2[i];
		if (i + z2[i] > r)
		{
			l = i;
			r = i + z2[i] - 1;
		}
	}

	vector<int> ans;
	for (int i = (int) p.length() + 1; i <= int(z1.size() - p.length()); ++i)
		if (z1[i] == (int) p.length() ||
				z1[i] + z2[z2.size() - i + 1] + 1 == (int) p.length())
			ans.push_back(i - (int) p.length());

	cout << ans.size() << "\n";
	for (int i : ans)
		cout << i << " ";

	return 0;
}
