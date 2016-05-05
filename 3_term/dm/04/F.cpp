#include <bits/stdc++.h>
using namespace std;

#define C(a, b) \
if (fl && find(graph[st[a]].begin(), graph[st[a]].end(), st[b]) == graph[st[a]].end())\
	fl = false;

vector<vector<int>> graph;

vector<int> st;
bool check1()
{
	if (st.size() == 6)
	{
		bool fl = true;
		C(0, 3);
		C(0, 4);
		C(0, 5);
		C(1, 3);
		C(1, 4);
		C(1, 5);
		C(2, 3);
		C(2, 4);
		C(2, 5);
		return !fl;
	}

	for (int i = 0; i < (int) graph.size(); ++i)
		if (find(st.begin(), st.end(), i) == st.end())
		{
			st.push_back(i);
			bool fl = check1();
			st.pop_back();
			if (!fl)
				return false;
		}
	return true;
}

bool check2()
{
	if (st.size() == 5)
	{
		bool fl = true;
		C(0, 1);
		C(0, 2);
		C(0, 3);
		C(0, 4);
		C(1, 2);
		C(1, 3);
		C(1, 4);
		C(2, 3);
		C(2, 4);
		//C(3, 4);
		return !fl;
	}

	for (int i = (st.size() == 0 ? 0 : (st.back() + 1));
			i <= int(graph.size() - (5 - st.size())); ++i)
		if (find(st.begin(), st.end(), i) == st.end())
		{
			st.push_back(i);
			bool fl = check2();
			st.pop_back();
			if (!fl)
				return false;
		}
	return true;
}

bool check3()
{
	if (graph.size() != 6)
		return true;
	
	int m = 0;
	for (auto& v : graph)
		m += (int) v.size();
	m /= 2;

	if (m != 11)
		return true;

	for (auto& v : graph)
		if (v.size() == 2 &&
				find(graph[v[0]].begin(), graph[v[0]].end(), v[1]) == graph[v[0]].end())
			return false;
	
	return true;
}

int main()
{
	ifstream cin("planaritycheck.in");
	ofstream cout("planaritycheck.out");

	int T;
	cin >> T;
	string s;
	getline(cin, s);
	for (int testI = 0; testI < T; ++testI)
	{
		//cin >> s;
		getline(cin, s);
		int n = (int) round((1 + sqrt(1 + s.length() * 8)) / 2);

		int sI = 0;
		graph.clear();
		graph.resize(n);
		for (int i = 0; i < n; ++i)
			for (int j = 0; j < i; ++j)
				if (s[sI++] == '1')
				{
					graph[i].push_back(j);
					graph[j].push_back(i);
				}

		if (check1() && check2() && check3())
			cout << "YES\n";
		else
			cout << "NO\n";
	}

	return 0;
}


/*

n * (n - 1) / 2 = x
n ^ 2 - n = 2x
n^2 - n - 2x = 0
D = 1 + 8x
n = (1 + sqrt(1 + 8x)) / 2

*/
