#include <bits/stdc++.h>
using namespace std;
typedef long long LL;
struct _ { ios_base::Init i; _() { cin.sync_with_stdio(0); cin.tie(0); } } _;

int main()
{
	int n;
	cin >> n;
	
	int last = 1 << n;
	for (int i = 0; i < last; ++i)
	{
		for (int j = i, number = 1; j > 0; j >>= 1, ++number)
			if (j & 1)
				cout << number << " ";
		cout << "\n";
	}

	return 0;
}
