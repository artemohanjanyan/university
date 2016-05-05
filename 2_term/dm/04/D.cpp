#include <iostream>
#include <fstream>
using namespace std;
struct _ { ios_base::Init i; _() { cin.sync_with_stdio(0); cin.tie(0); } } _;

const int maxn = 300000;

int p[maxn], index[maxn], size[maxn];
int n;

void init(int n)
{
	for (int i = 0; i < n; ++i)
	{
		p[i] = -1;
		size[i] = 1;
		index[i] = i;
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
	{
		p[a] = b;
		size[b] += size[a];
	}
	else
	{
		index[a] = index[b];
		p[b] = a;
		size[a] += size[b];
	}
}

int try_park(int i)
{
	int freeI = index[find(i)];
	merge(i, (freeI + 1) % n);
	return freeI;
}

int main()
{
	ifstream cin("parking.in");
	ofstream cout("parking.out");

	cin >> n;
	init(n);

	int tryI;
	for (int i = 0; i < n; ++i)
	{
		cin >> tryI;
		cout << try_park(tryI - 1) + 1 << " ";
	}

	return 0;
}
