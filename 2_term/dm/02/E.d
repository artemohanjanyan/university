import std.stdio;
import std.algorithm;

class Vector(T)
{
	private T[] elements;
	private size_t _size;
	private static const size_t DEFAULT_CAPACITY = 2;

	this()
	{
		elements = new T[DEFAULT_CAPACITY];
		_size = 0;
	}

	ref T opIndex(size_t i)
	{
		return elements[i];
	}

	size_t opDollar(size_t i)()
	{
		return _size;
	}

	void pushBack(T elem)
	{
		if (elements.length == size)
			elements.length = elements.length + elements.length / 2; // Extend factor
		elements[_size++] = elem;
	}

	void popBack()
	{
		// Reduce initiator factor
		if (_size * 3 <= elements.length)
			elements.length = max(DEFAULT_CAPACITY, elements.length / 2); // Reduce factor

		--_size;
	}

	@property size_t size()
	{
		return _size;
	}
}

class Stack(T)
{
	private Vector!T elements;

	this()
	{
		elements = new Vector!T();
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
		return elements[$ - 1];
	}

	bool isEmpty()
	{
		return elements.size == 0;
	}
}

int main()
{
	//File cin = stdin;
	//File cout = stdout;
	File cin = File("brackets.in", "r");
	File cout = File("brackets.out", "w");

	string s;
	while ((s = cin.readln()) !is null)
	{
		--s.length;

		Stack!char stack = new Stack!char;
		bool fl = true;
		foreach (char c; s)
			if (c == '(' || c == '[')
				stack.push(c);
			else
			{
				if (stack.isEmpty() ||
						c == ')' && stack.top != '(' ||
						c == ']' && stack.top != '[')
				{
					fl = false;
					break;
				}
				stack.pop();
			}
		fl &= stack.isEmpty();

		cout.writef(fl ? "YES\n" : "NO\n");
	}

	return 0;
}
