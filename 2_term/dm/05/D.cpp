#include <bits/stdc++.h>
using namespace std;
#define cout cerr

int64_t tree[1 << 18];
const size_t true_size = (1 << 17);
int32_t op_flag[1 << 18];
int64_t op_arg[1 << 18];

int64_t compose(int64_t a, int64_t b)
{
	return min(a, b);
}
int64_t f(int32_t op, int64_t arg1, int64_t arg2)
{
	if (op == 0)
		return arg1 + arg2;
	else
		return arg2;
}
int64_t get(size_t i)
{
	return f(op_flag[i], tree[i], op_arg[i]);
}
void update_value(size_t i)
{
	if (i * 2 + 2 < (1 << 18))
		tree[i] = compose(get(i * 2 + 1), get(i * 2 + 2));
	else
		tree[i] = get(i);
}
void push(size_t i)
{
	if (i * 2 + 2 < (1 << 18))
	{
		if (op_flag[i] == 0)
		{
			op_arg[i * 2 + 1] += op_arg[i];
			op_arg[i * 2 + 2] += op_arg[i];
		}
		else if (op_flag[i] == 1)
		{
			op_arg[i * 2 + 1] = op_arg[i];
			op_arg[i * 2 + 2] = op_arg[i];
		}

		if (op_flag[i] == 1)
			op_flag[i * 2 + 1] = op_flag[i * 2 + 2] = op_flag[i];
	}

	tree[i] = get(i);
	op_flag[i] = 0;
	op_arg[i] = 0;
}

void build(size_t i = 0, size_t l = 0, size_t r = true_size)
{
	if (l + 1 == r)
		return;

	build(i * 2 + 1, l, (l + r) / 2);
	build(i * 2 + 2, (l + r) / 2, r);
	update_value(i);
}

void update(int32_t op, size_t l, size_t r, int64_t new_value, size_t true_i = 0, size_t true_l = 0, size_t true_r = true_size)
{
	push(true_i);

	if (l == true_l && r == true_r)
	{
		op_flag[true_i] = op;
		op_arg[true_i] = new_value;
		return;
	}

	size_t mid = (true_l + true_r) / 2;
	if (r <= mid)
		update(op, l, r, new_value, true_i * 2 + 1, true_l, mid);
	else if (l >= mid)
		update(op, l, r, new_value, true_i * 2 + 2, mid, true_r);
	else
	{
		update(op, l, mid, new_value, true_i * 2 + 1, true_l, mid);
		update(op, mid, r, new_value, true_i * 2 + 2, mid, true_r);
	}

	update_value(true_i);
}

int64_t query(size_t l, size_t r, size_t i = 0, size_t true_l = 0, size_t true_r = true_size)
{
	push(i);

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
	ifstream cin("rmq2.in");
	ofstream cout("rmq2.out");

	size_t n;
	cin >> n;
	for (size_t i = 0; i < n; ++i)
		cin >> tree[i + true_size - 1];
	for (size_t i = n; i < true_size; ++i)
		tree[i + true_size - 1] = numeric_limits<int64_t>::max();

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
			size_t l, r;
			int64_t x;
			cin >> l >> r >> x;
			--l;
			update(command != "add", l, r, x);
		}

	return 0;
}
