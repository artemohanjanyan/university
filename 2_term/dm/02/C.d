import std.stdio;
import std.algorithm;

class Vector(T)
{
	private T[] elements;
	private size_t _size, begin, end;
	private static const size_t DEFAULT_CAPACITY = 8;

	this()
	{
		elements = new T[DEFAULT_CAPACITY];
		_size = 0;
		begin = end = 0;
	}

	ref T opIndex(size_t i)
	{
		return elements[i];
	}

	size_t opDollar(size_t i)()
	{
		return _size;
	}

	void push(T elem)
	{
		if (elements.length == size)
		{
			T[] newElements = new T[elements.length + elements.length / 2];
			ptrdiff_t offset = cast(ptrdiff_t) newElements.length - elements.length;
			foreach (size_t i; 0..end)
				newElements[i] = elements[i];
			foreach (size_t i; begin.._size)
				newElements[i + offset] = elements[i];
			begin += offset;
			elements = newElements;
		}

		elements[end] = elem;
		end = (end + 1) % elements.length;
		++_size;
	}

	void pop()
	{
		// Reduce initiator factor
		if (_size * 3 <= elements.length && elements.length > DEFAULT_CAPACITY)
		{
			T[] newElements = new T[max(DEFAULT_CAPACITY, elements.length / 2)];

			if (begin > end)
			{
				ptrdiff_t offset = cast(ptrdiff_t) newElements.length - elements.length;
				foreach (size_t i; 0..end)
					newElements[i] = elements[i];
				foreach (size_t i; begin..elements.length)
					newElements[i + offset] = elements[i];
				begin += offset;
			}
			else
			{
				foreach (size_t i; begin..end)
					newElements[i - begin] = elements[i];
				begin = 0;
				end = _size;
			}

			elements = newElements;
		}

		elements[begin] = 0;
		begin = (begin + 1) % elements.length;
		--_size;
	}

	T top()
	{
		return elements[begin];
	}

	@property size_t size()
	{
		return _size;
	}
}

int main()
{
	//File cin = stdin;
	//File cout = stdout;
	File cin = File("queue1.in", "r");
	File cout = File("queue1.out", "w");
	Vector!int queue = new Vector!int;

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
			queue.push(x);
		}
		else
		{
			cout.writef("%s\n", queue.top());
			queue.pop();
		}

		//cout.writeln(queue.elements);
		//cout.writef("%s\t\t%s %s\n", queue.size, queue.begin, queue.end);
		//cout.writeln();
	}

	return 0;
}
