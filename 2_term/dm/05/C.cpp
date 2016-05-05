#include <bits/stdc++.h>
using namespace std;

int32_t mod;
struct matrix
{
	int32_t a, b, c, d;

	matrix(int32_t a, int32_t b, int32_t c, int32_t d): a(a), b(b), c(c), d(d)
	{}

	matrix(): matrix(1, 0, 0, 1)
	{}
};

matrix operator*(const matrix &a, const matrix &b)
{
	return matrix(
			(a.a * b.a + a.b * b.c) % mod,
			(a.a * b.b + a.b * b.d) % mod,
			(a.c * b.a + a.d * b.c) % mod,
			(a.c * b.b + a.d * b.d) % mod);			
}

istream& operator>>(istream &in, matrix &a)
{
	return in >> a.a >> a.b >> a.c >> a.d;
}
ostream& operator<<(ostream &out, const matrix &a)
{
	return out << a.a << " " << a.b << "\n" << a.c << " " << a.d << "\n";
}

matrix tree[1 << 19];
const size_t true_size = (1 << 18);

matrix operation(matrix a, matrix b)
{
	return a * b;
}
const matrix neutral = matrix();

void update_value(size_t i)
{
	tree[i] = operation(tree[i * 2 + 1], tree[i * 2 + 2]);
}

void build(size_t i = 0, size_t l = 0, size_t r = true_size)
{
	if (l + 1 == r)
		return;

	build(i * 2 + 1, l, (l + r) / 2);
	build(i * 2 + 2, (l + r) / 2, r);
	update_value(i);
}

matrix query(size_t l, size_t r, size_t i = 0, size_t true_l = 0, size_t true_r = true_size)
{
	if (l == true_l && r == true_r)
		return tree[i];

	size_t mid = (true_l + true_r) / 2;
	if (r <= mid)
		return query(l, r, i * 2 + 1, true_l, mid);
	else if (l >= mid)
		return query(l, r, i * 2 + 2, mid, true_r);
	else
		return operation(query(l, mid, i * 2 + 1, true_l, mid), query(mid, r, i * 2 + 2, mid, true_r));
}

int main()
{
	ifstream cin("crypto.in");
	ofstream cout("crypto.out");

	size_t n, m;
	cin >> mod >> n >> m;
	for (size_t i = 0; i < n; ++i)
		cin >> tree[i + true_size - 1];

	build();
	size_t l, r;
	for (size_t i = 0; i < m; ++i)
	{
		cin >> l >> r;
		--l;
		cout << query(l, r) << "\n";
	}

	return 0;
}
