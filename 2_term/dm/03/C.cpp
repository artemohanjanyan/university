#include <bits/stdc++.h>
using namespace std;
struct _ { ios_base::Init i; _() { cin.sync_with_stdio(0); cin.tie(0); } } _;

struct Node;
size_t getSize(Node*);

struct Node
{
	int x;
	Node *left, *right, *parent;
	size_t size;

	Node(int x, Node *left = nullptr, Node *right = nullptr, Node *parent = nullptr)
			:x(x), left(left), right(right), parent(parent)
	{
		updateSize();
	}

	void updateSize()
	{
		size = getSize(left) + getSize(right) + 1;
	}

	size_t getI()
	{
		return getSize(left);
	}
};

size_t getSize(Node *v)
{
	if (v == nullptr)
		return 0;
	else
		return v->size;
}

bool hasParent(Node *v)
{
	return v->parent != nullptr;
}

bool hasGrandparent(Node *v)
{
	return hasParent(v) && hasParent(v->parent);
}

bool isLeftSon(Node *v)
{
	return v->parent->left == v;
}

bool isRightSon(Node *v)
{
	return v->parent->right == v;
}

void rotateLeft(Node *left)
{
	Node *parent = left->parent;

	parent->left = left->right;
	left->right = parent;

	left->parent = parent->parent;
	if (left->parent != nullptr)
	{
		if (isLeftSon(parent))
			left->parent->left = left;
		else
			left->parent->right = left;
	}
	parent->parent = left;
	if (parent->left != nullptr)
		parent->left->parent = parent;

	parent->updateSize();
	left->updateSize();
}

void rotateRight(Node *right)
{
	Node *parent = right->parent;

	parent->right = right->left;
	right->left = parent;

	right->parent = parent->parent;
	if (right->parent != nullptr)
	{
		if (isLeftSon(parent))
			right->parent->left = right;
		else
			right->parent->right = right;
	}
	parent->parent = right;
	if (parent->right != nullptr)
		parent->right->parent = parent;

	parent->updateSize();
	right->updateSize();
}

void splay(Node *v)
{
	while (hasParent(v))
		if (hasGrandparent(v))
			if (isLeftSon(v) && isLeftSon(v->parent))
			{
				rotateLeft(v->parent);
				rotateLeft(v);
			}
			else if (isRightSon(v) && isRightSon(v->parent))
			{
				rotateRight(v->parent);
				rotateRight(v);
			}
			else if (isLeftSon(v) && isRightSon(v->parent))
			{
				rotateLeft(v);
				rotateRight(v);
			}
			else
			{
				rotateRight(v);
				rotateLeft(v);
			}
		else
			if (isLeftSon(v))
				rotateLeft(v);
			else
				rotateRight(v);
}

Node* merge(Node *a, Node *b)
{
	if (a == nullptr)
		return b;

	while (a->right != nullptr)
		a = a->right;
	splay(a);
	a->right = b;
	if (b != nullptr)
	{
		b->parent = a;
		b->updateSize();
	}
	a->updateSize();

	return a;
}

void pushBack(Node *&a, int x)
{
	a = merge(a, new Node(x));
}

void find(Node *&a, size_t i)
{
	if (a == nullptr)
		return;

	while (true)
		if (a->getI() == i)
			break;
		else if (a->getI() > i)
			a = a->left;
		else if (a->right != nullptr)
		{
			i -= a->getI() + 1;
			a = a->right;
		}
		else
			break;

	splay(a);
}

pair<Node*, Node*> split(Node *a, size_t i)
{
	if (a == nullptr)
		return make_pair(nullptr, nullptr);

	find(a, i);

	if (a->getI() == i)
	{
		auto temp = make_pair(a->left, a);
		if (temp.first != nullptr)
		{
			temp.first->parent = nullptr;
			temp.first->updateSize();
		}
		temp.second->left = nullptr;
		temp.second->updateSize();
		return temp;
	}
	else
		return make_pair(a, nullptr);
}

void assign(Node *&a, size_t i, int x)
{
	find(a, i);
	a->x = x;
}

void sliceAndMTF(Node *&a, size_t l, size_t r)
{
	auto temp2 = split(a, r + 1);
	auto temp1 = split(temp2.first, l);
	a = merge(merge(temp1.second, temp1.first), temp2.second);
}

ostream& operator<<(ostream &out, Node *&v)
{
	if (v != nullptr)
	{
		size_t i = 0;
		while (find(v, i), v->getI() == i)
		{
			out << v->x << " ";
			++i;
		}
	}
	return out;
}

int main()
{
	ifstream cin("movetofront.in");
	ofstream cout("movetofront.out");

	Node *tree = nullptr;

	size_t n, m;
	cin >> n >> m;
	for (size_t i = 0; i < n; ++i)
		pushBack(tree, (int) i + 1);

	for (size_t i = 0; i < m; ++i)
	{
		int l, r;
		cin >> l >> r;
		--l, --r;
		sliceAndMTF(tree, l, r);
	}

	//for (size_t i = 0; i < n; ++i)
	//{
	//	find(tree, i);
	//	cout << tree->x << " ";
	//}
	cout << tree;

	return 0;
}
