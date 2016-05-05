import std.stdio;
import std.algorithm;

class Queue(T)
{
	private static class Node
	{
		Node prev, next;
		T value;

		this(Node prev, Node next, T value)
		{
			this.prev = prev;
			this.next = next;
			this.value = value;
		}
	}

	Node head, tail;

	this()
	{
		head = tail = null;
	}

	void push(T elem)
	{
		head = new Node(head, null, elem);
		if (head.prev !is null)
			head.prev.next = head;
		else
			tail = head;
	}

	void pop()
	{
		if (tail.next !is null)
			tail.next.prev = null;
		else
			head = null;
		tail = tail.next;
	}

	T top()
	{
		return tail.value;
	}
}

int main()
{
	//File cin = stdin;
	//File cout = stdout;
	File cin = File("queue2.in", "r");
	File cout = File("queue2.out", "w");
	Queue!int queue = new Queue!int;

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
	}

	return 0;
}
