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
	ifstream cin("brackets.in");
	ofstream cout("brackets.out");

	string s;
	Stack<char> a;

	while (getline(cin, s))
	{
		bool fl = true;
		for (size_t i = 0; i < s.length() && fl; ++i)
			if (s[i] == '(' || s[i] == '[')
				a.push(s[i]);
			else if (a.isEmpty() || (s[i] == ')' && a.top() != '(') || (s[i] == ']' && a.top() != '['))
				fl = false;
			else
				a.pop();

		fl &= a.isEmpty();

		cout << (fl ? "YES" : "NO") << "\n";
		a.clear();
	}

	return 0;
}
