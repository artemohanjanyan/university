#include <bits/stdc++.h>
using namespace std;

#define minn(a, b) ((a) < (b) ? (a) : (b))

template<class T>
class Stack
{
private:
	T maxValue;

	class Node
	{
	public:
		Node *next;
		T value;
		T min;
	};

	Node *head;

public:
	Stack(T maxV): maxValue(maxV), head(nullptr)
	{
	}

	void push(T elem)
	{
		if (head == nullptr)
			head = new Node{head, elem, minn(elem, maxValue)};
		else
			head = new Node{head, elem, minn(elem, head->min)};
	}

	T pop()
	{
		Node *oldHead = head;
		head = head->next;
		T ans = oldHead->value;
		delete oldHead;
		return ans;
	}

	T top()
	{
		return head->value;
	}

	T min()
	{
		if (head == nullptr)
			return maxValue;
		else
			return head->min;
	}

	bool isEmpty()
	{
		return head == nullptr;
	}
};

template<class T>
class Queue
{
private:
	Stack<T> first, second;

public:
	Queue(const T &maxV): first(maxV), second(maxV)
	{
	}

	void push(T elem)
	{
		first.push(elem);
	}

	T pop()
	{
		if (second.isEmpty())
			while (!first.isEmpty())
				second.push(first.pop());
		return second.pop();
	}

	T min()
	{
		return minn(first.min(), second.min());
	}
};

int32_t a, b, c;
size_t k;
int32_t *x;
int32_t getNext()
{
	static size_t i = 0;
	if (i < k)
		return x[i++];
	else
	{
		x[i % k] = a * x[(i - 2 + k) % k] + b * x[(i - 1 + k) % k] + c;
		return x[(i++ % k)];
	}
}

int main()
{
	ifstream cin("queuemin2.in");
	ofstream cout("queuemin2.out");

	size_t n, m;
	cin >> n >> m >> k;
	cin >> a >> b >> c;
	x = new int32_t[k];
	for (size_t i = 0; i < k; ++i)
		cin >> x[i];

	Queue<int32_t> queue(numeric_limits<int32_t>::max());
	for (size_t i = 0; i < m - 1; ++i)
		queue.push(getNext());

	int64_t ans = 0;
	for (size_t i = m - 1; i < n; ++i)
	{
		queue.push(getNext());
		ans += queue.min();
		queue.pop();
	}

	cout << ans << endl;

	return 0;
}
