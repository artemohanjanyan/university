#include <bits/stdc++.h>
using namespace std;
typedef long long LL;
struct _ { ios_base::Init i; _() { cin.sync_with_stdio(0); cin.tie(0); } } _;

const int maxn = 1000;

int main()
{
	//ifstream cin("huffman.in");
	//ofstream cout("huffman.out");

	int n;
	cin >> n;

	multiset<pair<LL, LL>> p;
	int x;

	for (int i = 0; i < n; ++i)
	{
		cin >> x;
		p.insert(make_pair(x, 0));
	}

	while (p.size() > 1)
	{
		auto a = *p.begin();
		auto b = *(++p.begin());
		p.erase(p.begin());
		p.erase(p.begin());

		auto c = make_pair(a.first + b.first, a.second + a.first + b.second + b.first);

		p.insert(c);
	}

	cout << p.begin()->second;

	return 0;
}
