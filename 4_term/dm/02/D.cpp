#include <bits/stdc++.h>
using namespace std;

int const alph = 'z' - 'a' + 1;

struct node
{
	int length;
	node *sufLink;
	node *next[alph] = {};
	int i;
	bool visited = false;
};

vector<pair<pair<int, int>, char>> ans;

int nodeN;
int edgeN;
void print(node *v)
{
	v->visited = true;
	v->i = ++nodeN;
	for (int i = 0; i < alph; ++i)
		if (v->next[i] != nullptr)
		{
			if (!v->next[i]->visited)
				print(v->next[i]);
			++edgeN;
			ans.push_back(make_pair(make_pair(v->i, v->next[i]->i), 'a' + i));
		}
}

int main()
{
	ifstream cin("automaton.in");
	ofstream cout("automaton.out");

	string s;
	cin >> s;

	node *last = new node{};
	last->length = 0;
	last->sufLink = nullptr;
	node *first = last;

	for (char c : s)
	{
		node *current = new node{};
		current->length = last->length + 1;

		node *it = last;
		for (; it != nullptr && it->next[c - 'a'] == nullptr; it = it->sufLink)
			it->next[c - 'a'] = current;

		if (it == nullptr)
			current->sufLink = first;
		else
		{
			node *q = it->next[c - 'a'];
			if (it->length + 1 == q->length)
				current->sufLink = q;
			else
			{
				node *clone = new node{};
				clone->length = it->length + 1;
				copy(q->next, q->next + alph, clone->next);
				clone->sufLink = q->sufLink;
				for (; it != nullptr && it->next[c - 'a'] == q; it = it->sufLink)
					it->next[c - 'a'] = clone;
				q->sufLink = current->sufLink = clone;
			}
		}

		last = current;
	}

	print(first);
	cout << nodeN << " " << edgeN << "\n";
	for (auto &e : ans)
		cout << e.first.first << " " << e.first.second << " " << e.second << "\n";

	vector<int> ans2;
	for (node *it = last; it != nullptr; it = it->sufLink)
		ans2.push_back(it->i);

	cout << ans2.size() << "\n";
	for (int num : ans2)
		cout << num << " ";

	return 0;
}
