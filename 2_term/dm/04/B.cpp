#include <iostream>
#include <fstream>
using namespace std;
struct _ { ios_base::Init i; _() { cin.sync_with_stdio(0); cin.tie(0); } } _;

const int maxn = 100000;

int p[maxn], mina[maxn], maxa[maxn], size[maxn];

void init(int n)
{
	for (int i = 0; i < n; ++i)
	{
		p[i] = -1;
		size[i] = 1;
		mina[i] = maxa[i] = i + 1;
	}
}

int find(int v)
{
	if (p[v] == -1)
		return v;
	return p[v] = find(p[v]);
}

void merge(int a, int b)
{
	a = find(a);
	b = find(b);
	if (a == b)
		return;

	if (size[a] < size[b])
		swap(a, b);
	p[b] = a;
	mina[a] = min(mina[a], mina[b]);
	maxa[a] = max(maxa[a], maxa[b]);
	size[a] += size[b];
}

int main()
{
	ifstream cin("dsu.in");
	ofstream cout("dsu.out");

	int n;
	cin >> n;
	init(n);

	string command;
	int a, b;
	while (cin >> command >> a)
		if (command == "get")
		{
			int r = find(a - 1);
			cout << mina[r] << " " << maxa[r] << " " << size[r] << "\n";
		}
		else
		{
			cin >> b;
			merge(a - 1, b - 1);
		}

	return 0;
}
