#include <iostream>
#include <fstream>
#include <set>
using namespace std;
typedef long long LL;
struct _ { ios_base::Init i; _() { cin.sync_with_stdio(0); cin.tie(0); } } _;

const int maxn = 20;

LL facts[maxn];

int main()
{
	ifstream cin("perm2num.in");
	ofstream cout("perm2num.out");

	LL n, k;
	cin >> n;

	facts[0] = 1;
	for (int i = 1; i < maxn; ++i)
		facts[i] = facts[i-1] * i;

	set<int> left;
	for (int i = 1; i <= n; ++i)
		left.insert(i);

	LL ans = 0;

	for (int i = 0; i < n; ++i)
	{
		int next;
		cin >> next;

		int j = 0;
		for (auto it = left.find(next); it != left.begin(); --it)
			++j;

		left.erase(next);

		ans += facts[n - i - 1] * j;
	}

	cout << ans;

	return 0;
}
