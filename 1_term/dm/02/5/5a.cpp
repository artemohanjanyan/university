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
	ifstream cin("num2perm.in");
	ofstream cout("num2perm.out");

	LL n, k;
	cin >> n >> k;

	facts[0] = 1;
	for (int i = 1; i < maxn; ++i)
		facts[i] = facts[i-1] * i;

	set<int> left;
	for (int i = 1; i <= n; ++i)
		left.insert(i);

	while (left.size() > 0)
	{
		LL acc = 0;
		for (auto it = left.begin(); it != left.end(); ++it, acc += facts[left.size() - 1])
			if (acc + facts[left.size() - 1] > k)
			{
				cout << *it << " ";
				left.erase(it);
				k -= acc;
				break;
			}
	}

	return 0;
}
