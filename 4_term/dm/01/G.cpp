#include <bits/stdc++.h>
using namespace std;

int const ALPH = 26;
int const MAXN = 1000000;

struct node
{
	node *next[ALPH] = {};

	node *parent;
	int parentI;

	node *suf;
	node *super_suf;
	vector<int> string_ids;

	node() {}
} * const root = new node{};

inline int getI(char c)
{
	return c - 'a';
}

bool ans[MAXN];

void print(node *v = root, int depth = 0)
{
	for (int i = 0; i < depth; ++i)
		cerr << " ";
	cerr << v << "   " << v->parentI << " " << v->suf << " " << v->super_suf << "   ";
	for (int string_id : v->string_ids)
		cerr << string_id << " ";
	cerr << "\n";

	for (node *next_node : v->next)
		if (next_node != nullptr)
			print(next_node, depth + 1);
}

int main()
{
	ifstream cin("search4.in");
	ofstream cout("search4.out");

	// read and create trie
	int n;
	cin >> n;
	for (int patternI = 0; patternI < n; ++patternI)
	{
		string s;
		cin >> s;
		node *v = root;
		int i = 0;
		for (; i < (int) s.length() && v->next[getI(s[i])] != nullptr; ++i)
			v = v->next[getI(s[i])];
		for (; i < (int) s.length(); ++i)
		{
			int nextI = getI(s[i]);
			v->next[nextI] = new node{};
			v->next[nextI]->parent = v;
			v->next[nextI]->parentI = nextI;
			v = v->next[nextI];
		}
		v->string_ids.push_back(patternI);
	}

	//suf links
	root->parent = nullptr;
	root->suf = root->super_suf = root;

	queue<node*> bfs_queue;
	for (node *next_node : root->next)
		if (next_node != nullptr)
			bfs_queue.push(next_node);
	while (!bfs_queue.empty())
	{
		node *v = bfs_queue.front(), *u = v->parent->suf;
		bfs_queue.pop();
		while (u != root && u->next[v->parentI] == nullptr)
			u = u->suf;
		v->suf = u != v->parent && u->next[v->parentI] != nullptr ? u->next[v->parentI] : root;
		if (v->suf->string_ids.size() == 0)
			v->super_suf = v->suf->super_suf;
		else
			v->super_suf = v->suf;
		for (node *next_node : v->next)
			if (next_node != nullptr)
				bfs_queue.push(next_node);
	}

	//go
	node *v = root;
	char c;
	while (cin >> c)
	{
		while (v != root && v->next[getI(c)] == nullptr)
			v = v->suf;
		v = v->next[getI(c)] == nullptr ? root : v->next[getI(c)];

		node *u = v;
		while (u != root)
		{
			for (int patternI : u->string_ids)
				ans[patternI] = true;
			u->string_ids.clear();
			u = u->super_suf;
		}
		v->super_suf = root;
	}

	for (int i = 0; i < n; ++i)
		cout << (ans[i] ? "YES\n" : "NO\n");

	return 0;
}
