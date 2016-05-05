#include <iostream>
#include <fstream>
#include <string>
using namespace std;
//#define cout cerr

const size_t table_size = 1000003;
const int64_t A = 1000000009, B = 23456789, mod = 1000003;

int64_t get_hash(int64_t x)
{
	return ((A * x + B) % mod + mod) % mod;
}

template<class T>
class List
{
private:
	struct Node
	{
		T value;
		Node *next, *previous;
	} *first, *last;

	void remove(Node *node)
	{
		if (node == first)
			first = first->next;
		if (node == last)
			last = last->previous;

		if (node->previous != nullptr)
			node->previous->next = node->next;
		if (node->next != nullptr)
			node->next->previous = node->previous;

		delete node;
	}

	Node* find(T elem)
	{
		for (Node *it = first; it != nullptr; it = it->next)
			if (it->value == elem)
				return it;
		return nullptr;
	}

public:
	List() : first(nullptr), last(nullptr)
	{}

	~List()
	{
		while (first != nullptr)
		{
			Node *next = first->next;
			delete first;
			first = next;
		}
	}

	void insert(T elem)
	{
		Node *it = find(elem);
		if (it != nullptr)
			return;

		if (first == nullptr)
			first = last = new Node{elem, nullptr, nullptr};
		else
		{
			last = new Node{elem, nullptr, last};
			last->previous->next = last;
		}
	}

	void remove(T elem)
	{
		Node *it = find(elem);
		if (it != nullptr)
			remove(it);
	}

	bool exists(T elem)
	{
		Node *it = find(elem);
		return it != nullptr;
	}
};

List<int32_t> table[table_size];

void insert(int32_t x)
{
	table[get_hash(x)].insert(x);
}

void remove(int32_t x)
{
	table[get_hash(x)].remove(x);
}

bool exists(int32_t x)
{
	return table[get_hash(x)].exists(x);
}

int main()
{
	ifstream cin("set.in");
	ofstream cout("set.out");

	string command;
	int32_t key;
	while (cin >> command >> key)
		if (command == "insert")
			insert(key);
		else if (command == "delete")
			remove(key);
		else
			cout << (exists(key) ? "true" : "false") << "\n";

	return 0;
}
