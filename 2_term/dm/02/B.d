import std.stdio;
import std.algorithm;

class Stack(T)
{
	private static class Node
	{
		Node next;
		T value;

		this(Node next, T value)
		{
			this.next = next;
			this.value = value;
		}
	}

	Node head;

	this()
	{
		head = null;
	}

	void push(T elem)
	{
		head = new Node(head, elem);
	}

	void pop()
	{
		head = head.next;
	}

	T top()
	{
		return head.value;
	}
}

int main()
{
	//File cin = stdin;
	//File cout = stdout;
	File cin = File("stack2.in", "r");
	File cout = File("stack2.out", "w");
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
