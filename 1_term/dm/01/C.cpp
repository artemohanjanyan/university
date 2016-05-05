#include <bits/stdc++.h>
using namespace std;
typedef long long LL;
struct _ { ios_base::Init i; _() { cin.sync_with_stdio(0); cin.tie(0); } } _;

int main()
{
	ifstream cin("mtf.in");
	ofstream cout("mtf.out");

	list<char> alphabet;

	for (char c = 'a'; c <= 'z'; ++c)
		alphabet.push_back(c);

	char c;
	while (cin >> c)
	{
		int ans = 1;
		for (auto it = alphabet.begin(); it != alphabet.end(); ++it)
			if (c != *it)
				++ans;
			else
			{
				alphabet.erase(it);
				break;
			}

		alphabet.push_front(c);

		cout << ans << " ";
	}

	return 0;
}
