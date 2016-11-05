#include <bits/stdc++.h>
using namespace std;

unordered_set<string> graph[26];
bool production[26];

int main()
{
	ifstream cin("epsilon.in");
	ofstream cout("epsilon.out");

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
		if (rhs.empty())
			production[lhs - 'A'] = true;
		else
			graph[lhs - 'A'].insert(rhs);
	}

	for (int i = 0; i < 26; ++i)
		for (int j = 0; j < 26; ++j)
			for (auto it = graph[j].begin(); !production[j] && it != graph[j].end(); ++it)
			{
				bool fl = true;
				for (int k = 0; fl && k < static_cast<int>(it->length()); ++k)
					fl &= !(it->at(k) >= 'a' && it->at(k) <= 'z') && production[it->at(k) - 'A'];
				production[j] = fl;
			}

	for (int i = 0; i < 26; ++i)
		if (production[i])
			cout << static_cast<char>('A' + i) << " ";

	return 0;
}
