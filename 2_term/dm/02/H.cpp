#include <bits/stdc++.h>
using namespace std;
struct _ { ios_base::Init i; _() { cin.sync_with_stdio(0); cin.tie(0); } } _;

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
		//reduceCapacity();
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

int32_t q[100000], reg[26];
size_t l, r;

int32_t get()
{
	return q[l++];
}

void put(int32_t x)
{
	q[r++] = x;
}

const uint32_t mod = 65536;

size_t findLabel(Vector<pair<string, size_t>> &labels, string &label)
{
	ptrdiff_t left = -1, right = labels.getSize();
	while (left + 1 < right)
	{
		ptrdiff_t mid = left + (right - left) / 2;
		if (labels[mid].first < label)
			left = mid;
		else
			right = mid;
	}

	return labels[right].second;
}

int main()
{
	ifstream cin("quack.in");
	ofstream cout("quack.out");

	Vector<string> commands;
	commands.pushBack("");
	//map<string, size_t> labels;
	Vector<pair<string, size_t>> labels;

	char command;
	string label;
	size_t i = 0;
	int32_t x, y;
	char r1, r2;

	while (getline(cin, commands[commands.getSize() - 1]))
	{
		stringstream str(commands[commands.getSize() - 1]);
		str >> command;
		if (command == ':')
		{
			str >> label;
			//labels[label] = commands.getSize() - 1;
			labels.pushBack(make_pair(label, commands.getSize() - 1));
		}

		commands.pushBack("");
	}
	commands.popBack();

	sort(labels.begin(), labels.end());

	while (i < commands.getSize())
	{
		stringstream str(commands[i]);
		str >> command;
		switch (command)
		{
			case '+':
				x = get();
				y = get();
				put((x + y) % mod);
				break;

			case '-':
				x = get();
				y = get();
				put((x - y + mod) % mod);
				break;

			case '*':
				x = get();
				y = get();
				put((int32_t) (((int64_t) x * y) % mod));
				break;

			case '/':
				x = get();
				y = get();
				put((x / y) % mod);
				break;

			case '%':
				x = get();
				y = get();
				put((x % y) % mod);
				break;

			case '>':
				str >> r1;
				reg[r1 - 'a'] = get();
				break;

			case '<':
				str >> r1;
				put(reg[r1 - 'a']);
				break;

			case 'P':
				str >> r1;
				if (str)
					cout << reg[r1 - 'a'];
				else
					cout << get();
				cout << "\n";
				break;

			case 'C':
				str >> r1;
				if (str)
					cout << (char) (reg[r1 - 'a'] % mod);
				else
					cout << (char) (get() % mod);
				break;

			case ':':
				break;

			case 'J':
				str >> label;
				//i = labels[label];
				i = findLabel(labels, label);
				break;

			case 'Z':
				str >> r1 >> label;
				if (reg[r1 - 'a'] == 0)
					//i = labels[label];
					i = findLabel(labels, label);
				break;

			case 'E':
				str >> r1 >> r2 >> label;
				if (reg[r1 - 'a'] == reg[r2 - 'a'])
					//i = labels[label];
					i = findLabel(labels, label);
				break;

			case 'G':
				str >> r1 >> r2 >> label;
				if (reg[r1 - 'a'] > reg[r2 - 'a'])
					//i = labels[label];
					i = findLabel(labels, label);
				break;

			case 'Q':
				i = commands.getSize();
				break;

			default:
				str.putback(command);
				str >> x;
				put(x);
				break;
		}

		++i;
	}

	return 0;
}
