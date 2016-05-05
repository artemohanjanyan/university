#include <bits/stdc++.h>
using namespace std;
struct _ { ios_base::Init i; _() { cin.sync_with_stdio(0); cin.tie(0); } } _;

int32_t tree[1 << 20];
const size_t true_size = (1 << 19);

int32_t compose(int32_t a, int32_t b)
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

void update(size_t i, int32_t new_value, size_t true_i = 0, size_t l = 0, size_t r = true_size)
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

int32_t query(size_t l, size_t r, size_t i = 0, size_t true_l = 0, size_t true_r = true_size)
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
	ifstream cin("rmq.in");
	ofstream cout("rmq.out");

	size_t n;
	cin >> n;
	for (size_t i = 0; i < n; ++i)
		cin >> tree[i + true_size - 1];
	for (size_t i = n; i < true_size; ++i)
		tree[i + true_size - 1] = numeric_limits<int32_t>::max();

	build();
	string command;
	while (cin >> command)
		if (command == "min")
		{
			size_t l, r;
			cin >> l >> r;
			--l;
			cout << query(l, r) << "\n";
		}
		else
		{
			size_t i;
			int32_t x;
			cin >> i >> x;
			update(i - 1, x);
		}

	return 0;
}
