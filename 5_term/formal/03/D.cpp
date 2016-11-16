#include <bits/stdc++.h>
using namespace std;

unordered_set<string> graph[26];

bool is_small(char c)
{
	return c >= 'a' && c <= 'z';
}

int const MOD = 1000000007;

long long d[26][100][100];

int main()
{
	ifstream cin("nfc.in");
	ofstream cout("nfc.out");

	int m;
	cin >> m;
	char starting_rule;
	cin >> starting_rule;
	for (int i = 0; i < m; ++i)
	{
		char lhs;
		string rhs;
		cin >> lhs;
		getline(cin, rhs);
		rhs = rhs.substr(min(4, (int) rhs.length()));
		graph[lhs - 'A'].insert(rhs);
	}

	string word;
	cin >> word;

	for (int i = 0; i < (int) word.length(); ++i)
		for (int j = 0; j < 26; ++j)
			for (auto const &rhs : graph[j])
				if (rhs.length() == 1 && rhs[0] == word[i])
				{
					d[j][i][i] = 1;
					break;
				}
	
	for (int i = 1; i < (int) word.length(); ++i)
		for (int j = 0; j + i < (int) word.length(); ++j)
		{
			int r = j + i;
			for (int lhs = 0; lhs < 26; ++lhs)
				for (auto const &rhs : graph[lhs])
					if (rhs.length() == 2)
						for (int k = 0; k < r; ++k)
							d[lhs][j][r] = (d[lhs][j][r] + d[rhs[0] - 'A'][j][k] * d[rhs[1] - 'A'][k + 1][r]) % MOD;
		}

	cout << d[starting_rule - 'A'][0][word.length() - 1];

	return 0;
}
