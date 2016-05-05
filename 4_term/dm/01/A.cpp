#include <bits/stdc++.h>
using namespace std;

int main()
{
	ifstream cin("search1.in");
	ofstream cout("search1.out");

	string s1, s2;
	cin >> s2 >> s1;

	vector<int> ans;

	for (int i = 0; i <= int(s1.length()) - int(s2.length()); ++i)
	{
		bool matches = true;
		for (int j = 0; j < (int) s2.length() && matches; ++j)
			matches &= (s1[i + j] == s2[j]);

		if (matches)
			ans.push_back(i + 1);
	}

	cout << ans.size() << "\n";
	for (int i : ans)
		cout << i << " ";

	return 0;
}
