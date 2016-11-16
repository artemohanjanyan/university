#include <bits/stdc++.h>
using namespace std;

int const MAX_N = 100000;

unordered_multimap<char, int> graph[MAX_N];
set<int> terms;

int main()
{
	ifstream cin("automaton.in");
	ofstream cout("automaton.out");

	char start_state;
	int m;
	cin >> m >> start_state;
	terms.insert(26);
	for (int i = 0; i < m; ++i)
	{
		char lhs;
		string tmp;
		string rhs;
		cin >> lhs >> tmp >> rhs;

		int from = lhs - 'A', to;
		if (rhs.length() == 1)
			to = 26;
		else
			to = rhs[1] - 'A';
		char c = rhs[0];
		graph[from].insert({c, to});
	}

	int n;
	cin >> n;

	for (int i = 0; i < n; ++i)
	{
		string s;
		cin >> s;

		set<int> states{start_state - 'A'};
		for (char c : s)
		{
			set<int> new_states{};
			for (int state : states)
				for (auto it = graph[state].find(c);
						it != graph[state].end() && it->first == c;
						++it)
					new_states.insert(it->second);
			states.swap(new_states);
		}

		if (states.count(26))
			cout << "yes\n";
		else
			cout << "no\n";
	}

	return 0;
}
