#include <bits/stdc++.h>
using namespace std;
struct _ { ios_base::Init i; _() { cin.sync_with_stdio(0); cin.tie(0); } } _;

int main()
{
	ifstream cin("isheap.in");
	ofstream cout("isheap.out");

	int n;
	cin >> n;
	vector<int> h(n);
	for (int i = 0; i < n; ++i)
		cin >> h[i];

	bool fl = true;
	for (int i = 0; i < n && fl; ++i)
	{
		if (2 * i + 1 < n && !(h[i] <= h[i * 2 + 1]))
			fl = false;
		if (2 * i + 2 < n && !(h[i] <= h[i * 2 + 2]))
			fl = false;
	}

	cout << (fl ? "YES" : "NO");
	
	return 0;
}
