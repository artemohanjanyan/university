#include <bits/stdc++.h>
using namespace std;

template<class T>
class Vector
{
private:
	const size_t MIN_CAPACITY = 8;

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
		moveElements(capacity + capacity / 2);
	}

	void reduceCapacity()
	{
		// Reduce initiator factor
		if (capacity == MIN_CAPACITY || size * 3 > capacity)
			return;

		// Reduce factor
		moveElements(max(capacity / 2, MIN_CAPACITY));
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
};

int main()
{
	//ifstream cin("stack1.in");
	//ofstream cout("stack1.out");

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
