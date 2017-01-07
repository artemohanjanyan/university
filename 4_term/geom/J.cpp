#include <bits/stdc++.h>
using namespace std;
struct _ { ios_base::Init i; _() { cin.sync_with_stdio(0); cin.tie(0); } } _;

int const MAX_N = 5000;

struct point
{
	int x, y;
};

long long exterior_product(point const &a, point const &b, point const &c)
{
	return static_cast<long long>(b.x - a.x) * (c.y - a.y) -
			static_cast<long long>(b.y - a.y) * (c.x - a.x);
}

int left_turn(point const &a, point const &b, point const &c)
{
	long long pr = exterior_product(a, b, c);
	return (pr > 0) - (pr < 0);
}

bool contains(point const &a, point const &b, point const &c, point const &d)
{
	int ab_turn = left_turn(a, b, d);
	int bc_turn = left_turn(b, c, d);
	int ca_turn = left_turn(c, a, d);
	return ab_turn != -1 && bc_turn != -1 && ca_turn != -1;
}

point points[MAX_N];

int list_next[MAX_N];
int list_prev[MAX_N];
bool list_removed[MAX_N];

void init_list(int n)
{
	for (int i = 1; i < n; ++i)
	{
		list_next[i - 1] = i;
		list_prev[i] = i - 1;
	}
	list_next[n - 1] = 0;
	list_prev[0] = n - 1;
}

int remove(int i)
{
	list_next[list_prev[i]] = list_next[i];
	list_prev[list_next[i]] = list_prev[i];
	list_removed[i] = true;
	return list_prev[i];
}

struct triangle_ids
{
	int a, b, c;
};

vector<triangle_ids> ans;

int main()
{
	int n;
	cin >> n;
	for (int i = 0; i < n; ++i)
		cin >> points[i].x >> points[i].y;

	init_list(n);

	int rest_n = n;

	for (int i = 0; rest_n > 3; i = list_next[i])
	{
		int ai = list_prev[i];
		int bi = i;
		int ci = list_next[i];
		point const &a = points[ai];
		point const &b = points[bi];
		point const &c = points[ci];

		if (left_turn(a, b, c) != 1)
			continue;

		bool fl = true;
		for (int j = 0; j < n; ++j)
			if (j != ai && j != bi && j != ci)
				fl &= !contains(a, b, c, points[j]);

		if (fl)
		{
			ans.push_back({ai, bi, ci});
			i = list_prev[remove(i)];
			--rest_n;
		}
	}
	
	for (auto const &triangle : ans)
		cout << triangle.a + 1 << " " << triangle.b + 1 << " " << triangle.c + 1 << "\n";
	for (int i = 0; i < n; ++i)
		if (!list_removed[i])
			cout << i + 1 << " ";
	cout << "\n";

	return 0;
}
