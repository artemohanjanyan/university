
#include <iostream>
#include <fstream>
#include <string>
using namespace std;
//#define cout cerr
 
template<class T>
class Maybe
{
private:
	bool somethingFlag;
	T something;
 
public:
	Maybe(): somethingFlag(false)
	{}
 
	Maybe(T _something): somethingFlag(true), something(_something)
	{}
 
	bool hasSomething()
	{
		return somethingFlag;
	}
 
	T getSomething()
	{
		return something;
	}
};
 
template<class KeyT, class ValueT>
class MapList
{
private:
	struct Node
	{
		KeyT key;
		ValueT value;
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
 
	Node* find(KeyT key)
	{
		for (Node *it = first; it != nullptr; it = it->next)
			if (it->key == key)
				return it;
		return nullptr;
	}
 
public:
	MapList() : first(nullptr), last(nullptr)
	{}
 
	~MapList()
	{
		while (first != nullptr)
		{
			Node *next = first->next;
			delete first;
			first = next;
		}
	}
 
	void insert(KeyT key, ValueT value)
	{
		Node *it = find(key);
		if (it != nullptr)
			return;
 
		if (first == nullptr)
			first = last = new Node{key, value, nullptr, nullptr};
		else
		{
			last = new Node{key, value, nullptr, last};
			last->previous->next = last;
		}
	}
 
	void remove(KeyT key)
	{
		Node *it = find(key);
		if (it != nullptr)
			remove(it);
	}
 
	Maybe<ValueT> get(KeyT key)
	{
		Node *it = find(key);
		if (it == nullptr)
			return Maybe<ValueT>();
		else
			return Maybe<ValueT>(it->value);
	}
};
 
template<class KeyT, class ValueT, size_t hash_function(KeyT key)>
class HashMap
{
private:
	static const size_t table_size = 1000003;
 
	MapList<KeyT, ValueT> table[table_size];
 
public:
	void insert(KeyT key, ValueT value)
	{
		table[hash_function(key) % table_size].remove(key);
		table[hash_function(key) % table_size].insert(key, value);
	}
 
	void remove(KeyT key)
	{
		table[hash_function(key) % table_size].remove(key);
	}
 
	Maybe<ValueT> get(KeyT key)
	{
		return table[hash_function(key) % table_size].get(key);
	}
};
 
size_t hash_function(string s)
{
	static const int64_t p = 277, mod = 1000000007;
	int64_t cur_p = 1, hash = 0;
	 
	for (char c : s)
	{
		hash = (hash + c * cur_p) % mod;
		cur_p = cur_p * p % mod;
	}
 
	return hash;
	//return s[0];
}
 
HashMap<string, string, hash_function> hashMap;
 
int main()
{
	ifstream cin("map.in");
	ofstream cout("map.out");
 
	string command, key;
	while (cin >> command >> key)
		if (command == "put")
		{
			string value;
			cin >> value;
			hashMap.insert(key, value);
		}
		else if (command == "delete")
			hashMap.remove(key);
		else
		{
			auto maybe = hashMap.get(key);
			if (maybe.hasSomething())
				cout << maybe.getSomething() << "\n";
			else
				cout << "none\n";
		}
 
	return 0;
}
