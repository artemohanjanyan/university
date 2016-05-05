#include <iostream>
#include <fstream>
#include <string>
using namespace std;
//#define cout cerr

struct Node1
{
	string value;
	Node1 *next, *previous;

	Node1(string _value = "",
			Node1 *_next = nullptr, Node1 *_previous = nullptr) :
					value(_value),
					next(_next), previous(_previous)
	{}

	~Node1()
	{
		if (next != nullptr)
			next->previous = previous;
		if (previous != nullptr)
			previous->next = next;
	}
};

struct MapList1
{
	Node1 *first, *last;
	size_t count;

	MapList1() : first(new Node1()), last(new Node1())
	{
		first->next = last;
		last->previous = first;
		count = 0;
	}

	~MapList1()
	{
		while (first->next != nullptr)
			delete first->next;
		delete first;
	}

	Node1* find(const string &value)
	{
		for (Node1 *it = first->next; it != last; it = it->next)
			if (it->value == value)
				return it;
		return nullptr;
	}

	void insert(const string &value)
	{
		Node1 *node = find(value);
		if (node != nullptr)
			return;
		
		++count;

		node = new Node1(value, first->next, first);

		first->next->previous = node;
		first->next = node;
	}

	void remove(const string &value)
	{
		Node1 *it = find(value);
		if (it != nullptr)
		{
			--count;
			delete it;
		}
	}
};

ostream& operator<<(ostream &out, MapList1 *list)
{
	out << list->count << " ";
	for (Node1 *it = list->first->next; it != list->last; it = it->next)
		out << it->value << " ";
	return out;
}

struct Node
{
	string key;
	MapList1 *list;
	Node *next, *previous;

	Node(string _key = "", string _value = "",
			Node *_next = nullptr, Node *_previous = nullptr) :
					key(_key), list(new MapList1()),
					next(_next), previous(_previous)
	{
		list->insert(_value);
	}

	~Node()
	{
		if (next != nullptr)
			next->previous = previous;
		if (previous != nullptr)
			previous->next = next;
		//delete list
	}
};

ostream& operator<<(ostream &out, Node *node)
{
	if (node == nullptr)
		return out << 0;
	else
		return out << node->list;
}

struct MapList
{
	Node *first, *last;

	MapList() : first(new Node()), last(new Node())
	{
		first->next = last;
		last->previous = first;
	}

	//~MapList()
	//{
	//	while (first->next != nullptr)
	//		delete first->next;
	//	delete first;
	//}

	Node* find(const string &key)
	{
		for (Node *it = first->next; it != last; it = it->next)
			if (it->key == key)
				return it;
		return nullptr;
	}

	void insert(const string &key, const string &value)
	{
		Node *node = find(key);
		if (node != nullptr)
		{
			node->list->insert(value);
			return;
		}

		node = new Node(key, value, first->next, first);

		first->next->previous = node;
		first->next = node;
	}

	void remove(string key)
	{
		Node *it = find(key);
		if (it != nullptr)
			delete it;
	}

	void remove(string key, string value)
	{
		Node *it = find(key);
		if (it != nullptr)
			it->list->remove(value);
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
	//static const size_t table_size = 1000003;
	static const size_t table_size = 300007;

	MapList table[table_size];

	void insert(string key, string value)
	{
		table[hash_function(key) % table_size].insert(key, value);
	}

	void remove(string key)
	{
		table[hash_function(key) % table_size].remove(key);
	}

	void remove(string key, string value)
	{
		table[hash_function(key) % table_size].remove(key, value);
	}

	Node* get(string key)
	{
		return table[hash_function(key) % table_size].find(key);
	}
};

HashMap hashMap;

int main()
{
	ifstream cin("multimap.in");
	ofstream cout("multimap.out");

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
			string value;
			cin >> value;
			hashMap.remove(key, value);
		}
		else if (command == "deleteall")
		{
			hashMap.remove(key);
		}
		else if (command == "get")
		{
			Node *node = hashMap.get(key);
			cout << node << "\n";
		}

	return 0;
}
