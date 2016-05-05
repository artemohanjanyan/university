#include <bits/stdc++.h>
using namespace std;
typedef long long LL;
struct _ { ios_base::Init i; _() { cin.sync_with_stdio(0); cin.tie(0); } } _;

const int maxn = 1000;

string a[maxn];

int main()
{
	ifstream cin("bwt.in");
	ofstream cout("bwt.out");

	string s;
	cin >> s;

	a[0] = s;
	for (int i = 1; i < s.length(); ++i)
		a[i] = s.substr(i) + s.substr(0, i);

	sort(a, a + s.length());

	for (int i = 0; i < s.length(); ++i)
		cout << a[i][s.length() - 1];

	return 0;
}
