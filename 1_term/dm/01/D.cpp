#include <bits/stdc++.h>
using namespace std;
typedef long long LL;
struct _ { ios_base::Init i; _() { cin.sync_with_stdio(0); cin.tie(0); } } _;

class Node
{
public:
	int number;
	Node *next[26];

	Node(int _number): number(_number)
	{
		fill(next, next + 26, nullptr);
	}

	Node*& getNext(char c)
	{
		return next[c - 'a'];
	}

	~Node()
	{
		for (auto ptr : next)
			delete ptr;
	}
};

int main()
{
	//ifstream cin("lzw.in");
	//ofstream cout("lzw.out");

	Node *head = new Node(-1);

	for (char c = 'a'; c <= 'z'; ++c)
		head->getNext(c) = new Node(c - 'a');

	int nextNumber = 26;
	char c;
	Node *cur = head;

	while (cin >> c)
	{
		if (cur->getNext(c) != nullptr)
			cur = cur->getNext(c);
		else
		{
			cur->getNext(c) = new Node(nextNumber++);
			cout << cur->number << " ";
			cur = head->getNext(c);
		}
	}

	if (cur->number != -1)
		cout << cur->number;

	return 0;
}
