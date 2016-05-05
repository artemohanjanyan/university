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
		elements = new Vector!int();
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
}

int main()
{
	//File cin = stdin;
	//File cout = stdout;
	File cin = File("stack1.in", "r");
	File cout = File("stack1.out", "w");
	Stack!int stack = new Stack!int;

	size_t n;
	cin.readf(" %s", &n);
	foreach (size_t opI; 0..n)
	{
		char op;
		cin.readf(" %s", &op);
		if (op == '+')
		{
			int x;
			cin.readf(" %s", &x);
			stack.push(x);
		}
		else
		{
			cout.writef("%s\n", stack.top());
			stack.pop();
		}
	}

	return 0;
}
