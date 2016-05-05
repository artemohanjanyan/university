#include <bits/stdc++.h>
using namespace std;
#define cout cerr

size_t tree[1 << 20];
const size_t true_size = (1 << 19);
const size_t neutral = numeric_limits<size_t>::max();

size_t compose(size_t a, size_t b)
{
	return min(a, b);
}
void update_value(size_t i)
{
	tree[i] = compose(tree[i * 2 + 1], tree[i * 2 + 2]);
}

void build(size_t i = 0, size_t l = 0, size_t r = true_size)
{
	if (l + 1 == r)
		return;

	build(i * 2 + 1, l, (l + r) / 2);
	build(i * 2 + 2, (l + r) / 2, r);
	update_value(i);
}

void update(size_t i, size_t new_value, size_t true_i = 0, size_t l = 0, size_t r = true_size)
{
	if (l + 1 == r)
	{
		tree[true_i] = new_value;
		return;
	}

	size_t mid = (l + r) / 2;
	if (i < mid)
		update(i, new_value, true_i * 2 + 1, l, mid);
	else
		update(i, new_value, true_i * 2 + 2, mid, r);
	update_value(true_i);
}

size_t query(size_t l, size_t r, size_t i = 0, size_t true_l = 0, size_t true_r = true_size)
{
	if (l == true_l && r == true_r)
		return tree[i];

	size_t mid = (true_l + true_r) / 2;
	if (r <= mid)
		return query(l, r, i * 2 + 1, true_l, mid);
	else if (l >= mid)
		return query(l, r, i * 2 + 2, mid, true_r);
	else
		return compose(query(l, mid, i * 2 + 1, true_l, mid), query(mid, r, i * 2 + 2, mid, true_r));
}

int main()
{
	ifstream cin("parking.in");
	ofstream cout("parking.out");

	size_t n, m;
	cin >> n >> m;
	for (size_t i = 0; i < n; ++i)
		tree[i + true_size - 1] = i;
	for (size_t i = n; i < true_size; ++i)
		tree[i + true_size - 1] = neutral;

	build();
	string command;
	while (cin >> command)
		if (command == "enter")
		{
			size_t i;
			cin >> i;
			--i;
			size_t q = query(i, n);
			if (q == neutral)
				q = query(0, i);
			cout << q + 1 << "\n";
			update(q, neutral);
		}
		else
		{
			size_t i;
			cin >> i;
			update(i - 1, i - 1);
		}

	return 0;
}
