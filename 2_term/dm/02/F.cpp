#include <bits/stdc++.h>
using namespace std;

template<class T>
class Vector
{
private:
	const size_t MIN_CAPACITY = 64;

	T *elements;
	size_t size, capacity;

	void moveElements(size_t newCapacity)
	{
		T *newElements = new T[newCapacity];
		for (size_t i = 0; i < size; ++i)
			newElements[i] = elements[i];
		delete[] elements;
		elements = newElements;
		capacity = newCapacity;
	}

	void extendCapacity()
	{
		if (size < capacity)
			return;

		// Extend factor
		//moveElements(capacity + capacity / 2);
		moveElements(capacity + capacity);
	}

	void reduceCapacity()
	{
		//// Reduce initiator factor
		//if (capacity == MIN_CAPACITY || size * 3 > capacity)
		//	return;

		//// Reduce factor
		//moveElements(max(capacity / 2, MIN_CAPACITY));
	}

public:
	Vector(): size(0), capacity(MIN_CAPACITY)
	{
		elements = new T[capacity];
	}

	~Vector()
	{
		delete[] elements;
	}

	T& operator[](size_t i) const
	{
		return elements[i];
	}

	void pushBack(T elem)
	{
		extendCapacity();
		elements[size++] = elem;
	}

	void popBack()
	{
		--size;
		reduceCapacity();
	}

	T* begin() const
	{
		return elements;
	}

	T* end() const
	{
		return elements + size;
	}

	size_t getSize() const
	{
		return size;
	}

	bool isEmpty()
	{
		return size == 0;
	}

	void clear()
	{
		size = 0;
	}
};

template<class T>
class Stack
{
private:
	Vector<T> elements;

public:
	Stack()
	{
	}

	void push(T elem)
	{
		elements.pushBack(elem);
	}

	void pop()
	{
		elements.popBack();
	}

	T top()
	{
		return elements[elements.getSize() - 1];
	}

	bool isEmpty()
	{
		return elements.isEmpty();
	}

	void clear()
	{
		elements.clear();
	}
};

int main()
{
	ifstream cin("postfix.in");
	ofstream cout("postfix.out");

	Stack<int> stack;
	char c;
	int n;

	while (cin >> c)
		if (isdigit(c))
		{
			cin.putback(c);
			cin >> n;
			stack.push(n);
		}
		else
		{
			int b = stack.top();
			stack.pop();
			int a = stack.top();
			stack.pop();
			if (c == '+')
				stack.push(a + b);
			else if (c == '-')
				stack.push(a - b);
			else if (c == '*')
				stack.push(a * b);
		}

	cout << stack.top();

	return 0;
}
