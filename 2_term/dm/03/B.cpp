#include <bits/stdc++.h>
using namespace std;
struct _ { ios_base::Init i; _() { cin.sync_with_stdio(0); cin.tie(0); } } _;

struct Node
{
	int x, y;
	Node *left, *right;
	Node(int x, int y, Node *left = nullptr, Node *right = nullptr):
			x(x), y(y), left(left), right(right) {}
};

Node* merge(Node *a, Node *b)
{
	if (a == nullptr)
		return b;
	if (b == nullptr)
		return a;
	if (a->y < b->y)
		return new Node(a->x, a->y, a->left, merge(a->right, b));
	else
		return new Node(b->x, b->y, merge(a, b->left), b->right);
}

pair<Node*, Node*> split(Node *a, int key)
{
	if (a == nullptr)
		return make_pair(nullptr, nullptr);
	if (a->x < key)
	{
		auto right = split(a->right, key);
		return make_pair(new Node(a->x, a->y, a->left, right.first), right.second);
	}
	else
	{
		auto left = split(a->left, key);
		return make_pair(left.first, new Node(a->x, a->y, left.second, a->right));
	}
}

Node* insert(Node *a, int key)
{
	auto temp = split(a, key);
	return merge(merge(temp.first, new Node(key, rand())), temp.second);
}

Node* erase(Node *a, int key)
{
	auto temp1 = split(a, key);
	auto temp2 = split(temp1.second, key + 1);
	return merge(temp1.first, temp2.second);
}

bool exists(Node *a, int key)
{
	if (a == nullptr)
		return false;
	if (a->x == key)
		return true;
	if (a->x < key)
		return exists(a->right, key);
	return exists(a->left, key);
}

Node* next(Node *a, int key)
{
	if (a == nullptr)
		return a;
	if (a->x > key)
	{
		auto left = next(a->left, key);
		if (left == nullptr)
			return a;
		else
			return left;
	}
	else
		return next(a->right, key);
}

Node* prev(Node *a, int key)
{
	if (a == nullptr)
		return a;
	if (a->x < key)
	{
		auto right = prev(a->right, key);
		if (right == nullptr)
			return a;
		else
			return right;
	}
	else
		return prev(a->left, key);
}

int main()
{
	ifstream cin("bst.in");
	ofstream cout("bst.out");

	srand((unsigned int) time(0));

	string command;
	int key;
	Node *treap = nullptr;

	while (cin >> command >> key)
		if (command == "insert")
			treap = insert(treap, key);
		else if (command == "delete")
			treap = erase(treap, key);
		else if (command == "exists")
			cout << (exists(treap, key) ? "true\n" : "false\n");
		else if (command == "next")
		{
			auto temp = next(treap, key);
			if (temp == nullptr)
				cout << "none\n";
			else
				cout << temp->x << "\n";
		}
		else
		{
			auto temp = prev(treap, key);
			if (temp == nullptr)
				cout << "none\n";
			else
				cout << temp->x << "\n";
		}

	return 0;
}
