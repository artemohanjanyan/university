#include <iostream>
#include <fstream>
#include <string>
using namespace std;
//#define cout cerr

struct Node
{
	string key;
	string value;
	Node *next, *previous;

	Node *nextQuery, *previousQuery;

	Node(string _key = "", string _value = "",
			Node *_next = nullptr, Node *_previous = nullptr,
			Node *_nextQuery = nullptr, Node *_previousQuery = nullptr) :
					key(_key), value(_value),
					next(_next), previous(_previous),
					nextQuery(_nextQuery), previousQuery(_previousQuery)
	{}

	~Node()
	{
		if (next != nullptr)
			next->previous = previous;
		if (previous != nullptr)
			previous->next = next;

		if (nextQuery != nullptr)
			nextQuery->previousQuery = previousQuery;
		if (previousQuery != nullptr)
			previousQuery->nextQuery = nextQuery;
	}
};

struct MapList
{
	Node *first, *last;

	MapList() : first(new Node()), last(new Node())
	{
		first->next = last;
		last->previous = first;
	}

	~MapList()
	{
		while (first->next != nullptr)
			delete first->next;
		delete first;
	}

	Node* find(const string &key)
	{
		for (Node *it = first->next; it != last; it = it->next)
			if (it->key == key)
				return it;
		return nullptr;
	}

	void insert(const string &key, const string &value, Node *lastQuery)
	{
		Node *node = find(key);
		if (node != nullptr)
		{
			node->value = value;
			return;
		}

		node = new Node(key, value, first->next, first, lastQuery, lastQuery->previousQuery);
		lastQuery->previousQuery->nextQuery = node;
		lastQuery->previousQuery = node;

		first->next->previous = node;
		first->next = node;
	}

	void remove(string key)
	{
		Node *it = find(key);
		if (it != nullptr)
			delete it;
	}
};

size_t hash_function(const string &s)
{
	static const int64_t p = 277, mod = 1000000007;
	int64_t cur_p = 1, hash = 0;

	for (char c : s)
	{
		hash = (hash + c * cur_p) % mod;
		cur_p = cur_p * p % mod;
	}

	return hash;
}

struct HashMap
{
	static const size_t table_size = 1000003;

	MapList table[table_size];
	Node *firstQuery, *lastQuery;

	HashMap() : firstQuery(new Node()), lastQuery(new Node())
	{
		firstQuery->nextQuery = lastQuery;
		lastQuery->previousQuery = firstQuery;
	}

	~HashMap()
	{
		delete firstQuery;
		delete lastQuery;
	}

	void insert(string key, string value)
	{
		//table[hash_function(key) % table_size].remove(key);
		table[hash_function(key) % table_size].insert(key, value, lastQuery);
	}

	void remove(string key)
	{
		table[hash_function(key) % table_size].remove(key);
	}

	Node* get(string key)
	{
		return table[hash_function(key) % table_size].find(key);
	}

	Node* prev(string key)
	{
		Node *node = get(key);
		if (node != nullptr)
			node = node->previousQuery;

		if (node == firstQuery)
			node = nullptr;

		return node;
	}

	Node* next(string key)
	{
		Node *node = get(key);
		if (node != nullptr)
			node = node->nextQuery;

		if (node == lastQuery)
			node = nullptr;

		return node;
	}
};

HashMap hashMap;

int main()
{
	ifstream cin("linkedmap.in");
	ofstream cout("linkedmap.out");

	string command, key;
	while (cin >> command >> key)
		if (command == "put")
		{
			string value;
			cin >> value;

			hashMap.insert(key, value);
		}
		else if (command == "delete")
		{
			hashMap.remove(key);
		}
		else if (command == "get")
		{
			Node *node = hashMap.get(key);
			if (node != nullptr)
				cout << node->value << "\n";
			else
				cout << "none\n";
		}
		else if (command == "prev")
		{
			Node *node = hashMap.prev(key);
			if (node != nullptr)
				cout << node->value << "\n";
			else
				cout << "none\n";
		}
		else
		{
			Node *node = hashMap.next(key);
			if (node != nullptr)
				cout << node->value << "\n";
			else
				cout << "none\n";
		}

	return 0;
}
