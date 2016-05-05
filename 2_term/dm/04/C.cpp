#include <bits/stdc++.h>
using namespace std;
struct _ { ios_base::Init i; _() { cin.sync_with_stdio(0); cin.tie(0); } } _;

#define pre(v) (((v) - 1) / 2)
#define next1(v) ((v) * 2 + 1)
#define next2(v) ((v) * 2 + 2)

const int maxn = 1000000;
int heap[maxn], size;
int direct[maxn], back[maxn];

void swap_nodes(int i1, int i2)
{
	swap(heap[i1], heap[i2]);
	swap(direct[back[i1]], direct[back[i2]]);
	swap(back[i1], back[i2]);
}

void sift_up(int v)
{
	while (v > 0 && heap[pre(v)] > heap[v])
	{
		swap_nodes(v, pre(v));
		v = pre(v);
	}
}

void sift_down(int v)
{
	while (next1(v) < size)
	{
		int swapV = v;
		if (heap[next1(v)] < heap[swapV])
			swapV = next1(v);
		if (next2(v) < size && heap[next2(v)] < heap[swapV])
			swapV = next2(v);

		if (swapV != v)
		{
			swap_nodes(v, swapV);
			v = swapV;
		}
		else
			break;
	}
}

int extract_min()
{
	swap_nodes(0, size - 1);
	--size;
	sift_down(0);
	direct[back[size]] = -1;
	return heap[size];
}

void push(int key, int opN)
{
	heap[size] = key;
	back[size] = opN;
	direct[opN] = size;
	++size;
	sift_up(size - 1);
}

void decrease_key(int opN, int dKey)
{
	if (direct[opN] == -1)
		return;
	int v = direct[opN];
	heap[v] = dKey;
	sift_up(v);
	sift_down(v);
}

int main()
{
	ifstream cin("priorityqueue.in");
	ofstream cout("priorityqueue.out");

	size = 0;
	string command;
	for (int i = 1; cin >> command; ++i)
		if (command == "push")
		{
			int key;
			cin >> key;
			push(key, i);
		}
		else if (command == "extract-min")
		{
			if (size == 0)
				cout << "*\n";
			else
				cout << extract_min() << "\n";
		}
		else
		{
			int op, dKey;
			cin >> op >> dKey;
			decrease_key(op, dKey);
		}

	return 0;
}
