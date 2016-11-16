#include <bits/stdc++.h>
using namespace std;

unordered_set<string> graph[26];

bool present[26];

bool is_small(char c)
{
	return c >= 'a' && c <= 'z';
}

int main()
{
	//ifstream cin("useless.in");
	//ofstream cout("useless.out");

	int m;
	cin >> m;
	char starting_rule;
	cin >> starting_rule;
	present[starting_rule - 'A'] = true;
	for (int i = 0; i < m; ++i)
	{
		char lhs;
		string rhs;
		cin >> lhs;
		getline(cin, rhs);
		rhs = rhs.substr(min(4, (int) rhs.length()));
		graph[lhs - 'A'].insert(rhs);

		present[lhs - 'A'] = true;
		for (char c : rhs)
			if (c >= 'A' && c <= 'Z')
				present[c - 'A'] = true;
	}

	// Not producing

	bool producing[26] = {};
	for (int i = 0; i < 26; ++i)
		for (string const &rhs : graph[i])
		{
			bool fl = true;
			for (char c : rhs)
				fl &= is_small(c);
			if (fl)
			{
				producing[i] = true;
				break;
			}
		}

	for (int i = 0; i < 26; ++i)
		for (int j = 0; j < 26; ++j)
			if (!producing[j])
				for (string const &rhs : graph[j])
				{
					bool fl = true;
					for (char c : rhs)
						fl &= is_small(c) || producing[c - 'A'];
					if (fl)
					{
						producing[j] = true;
						break;
					}
				}

	for (int i = 0; i < 26; ++i)
		if (!producing[i])
			graph[i].clear();
		else
			for (auto it = graph[i].begin(); it != graph[i].end(); )
			{
				bool fl = true;
				for (char c : *it)
					fl &= is_small(c) || producing[c - 'A'];
				if (!fl)
					it = graph[i].erase(it);
				else
					++it;
			}

	// Not reachable

	bool reachable[26] = {};
	reachable[starting_rule - 'A'] = true;

	for (int i = 0; i < 26; ++i)
		for (int j = 0; j < 26; ++j)
			if (reachable[j])
				for (string const &rhs : graph[j])
					for (char c : rhs)
						if (!is_small(c))
							reachable[c - 'A'] = true;

	for (int i = 0; i < 26; ++i)
		if (present[i] && (!producing[i] || !reachable[i]))
			cout << static_cast<char>('A' + i) << " ";

	return 0;
}
