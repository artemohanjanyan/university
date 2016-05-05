#include <bits/stdc++.h>
using namespace std;

template<class T>
class Stack
{
private:
	class Node
	{
	public:
		Node *next;
		T value;
	};

	Node *head;

public:
	Stack(): head(nullptr)
	{}

	void push(T elem)
	{
		head = new Node{head, elem};
	}

	void pop()
	{
		Node *oldHead = head;
		head = head->next;
		delete oldHead;
	}

	T top()
	{
		return head->value;
	}
};

int main()
{
	ifstream cin("stack2.in");
	ofstream cout("stack2.out");

	size_t n;
	cin >> n;
	Stack<int> a;

	for (size_t i = 0; i < n; ++i)
	{
		char command;
		cin >> command;
		if (command == '+')
		{
			int x;
			cin >> x;
			a.push(x);
		}
		else
		{
			cout << a.top() << "\n";
			a.pop();
		}
	}

	return 0;
}
