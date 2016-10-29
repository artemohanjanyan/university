#include <bits/stdc++.h>
using namespace std;

int const MAX_N = 100000;

unordered_map<char, int> graph[MAX_N];
vector<int> terms;

int main()
{
	ifstream cin("problem1.in");
	ofstream cout("problem1.out");

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
		graph[from - 1][c] = to - 1;
	}

	bool accepts = true;
	int v = 0;
	for (char c : s)
		if (graph[v].count(c))
			v = graph[v][c];
		else
		{
			accepts = false;
			break;
		}

	accepts &= find(terms.begin(), terms.end(), v + 1) != terms.end();

	cout << (accepts ? "Accepts" : "Rejects");

	return 0;
}
