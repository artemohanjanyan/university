#include <bits/stdc++.h>
using namespace std;

int main()
{
	ifstream cin("prefix.in");
	ofstream cout("prefix.out");

	string s;
	cin >> s;

	vector<int> pi(s.length());
	pi[0] = 0;

	for (int i = 1; i < (int) s.length(); ++i)
	{
		int k = pi[i - 1];
		while (k > 0 && s[i] != s[k])
			k = pi[k - 1];
		pi[i] = k + (s[i] == s[k]);
	}

	for (int l : pi)
		cout << l << " ";
	
	return 0;
}
