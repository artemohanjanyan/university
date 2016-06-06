#include <bits/stdc++.h>
using namespace std;

int const MAXN = 100000;
int a[MAXN], b[MAXN];

priority_queue<pair<int, int>, vector<pair<int, int>>, greater<pair<int, int>>> heap;

int main()
{
	freopen("f2cmax.in", "r", stdin);
	freopen("f2cmax.out", "w", stdout);

	ios_base::sync_with_stdio(false);
	cin.tie(NULL);

	int n;
	cin >> n;
	for (int i = 0; i < n; ++i)
		cin >> a[i];
	for (int i = 0; i < n; ++i)
		cin >> b[i];

	for (int i = 0; i < n; ++i)
		heap.push(make_pair(min(a[i], b[i]), i));

	vector<int> l, r;
	while (!heap.empty())
	{
		auto p = heap.top();
		heap.pop();
		(p.first == a[p.second] ? l : r).push_back(p.second);
	}

	l.insert(l.end(), r.rbegin(), r.rend());

	long long time1 = 0, time2 = 0;
	for (int i = 0; i < n; ++i)
	{
		time1 += a[l[i]];
		time2 += max(time1 - time2, 0ll) + b[l[i]];
	}
	
	cout << time2 << "\n";
	for (auto x : l)
		cout << x + 1 << " ";
	cout << "\n";
	for (auto x : l)
		cout << x + 1 << " ";
	cout << "\n";

	return 0;
}
