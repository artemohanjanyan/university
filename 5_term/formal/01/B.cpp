#include <bits/stdc++.h>
using namespace std;

int const MAX_N = 100000;

unordered_multimap<char, int> graph[MAX_N];
vector<int> terms;

int main()
{
	ifstream cin("problem2.in");
	ofstream cout("problem2.out");

	string s;
	cin >> s;

	int n, m, k;
	cin >> n >> m >> k;
	terms.resize(k);
	for (int &term : terms)
		cin >> term;
	for (int i = 0; i < m; ++i)
	{
		int from, to;
		char c;
		cin >> from >> to >> c;
		graph[from - 1].insert({c, to - 1});
	}

	set<int> states{0};
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

	for (int state : states)
		if (find(terms.begin(), terms.end(), state + 1) != terms.end())
		{
			cout << "Accepts\n";
			return 0;
		}

	cout << "Rejects\n";
	return 0;
}
