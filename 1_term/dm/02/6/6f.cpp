#include <iostream>
#include <fstream>
using namespace std;
typedef long long LL;
struct _ { ios_base::Init i; _() { cin.sync_with_stdio(0); cin.tie(0); } } _;

const int maxn = 100000;

int a[maxn];

bool nextPermutation(int *begin, int *end)
{
	int *i = end;
	while (begin < --i)
		if (*(i - 1) < *i)
		{
			int *j = i;
			--i;
			while (j < end && *i < *j)
				++j;
			--j;

			swap(*i, *j);

			for (++i, j = end - 1; i < j; ++i, --j)
				swap(*i, *j);

			return true;
		}

	for (--end; begin < end; ++begin, --end)
		swap(*begin, *end);

	return false;
}

int main()
{
	ifstream cin("nextmultiperm.in");
	ofstream cout("nextmultiperm.out");

	int n;
	cin >> n;
	for (int i = 0; i < n; ++i)
		cin >> a[i];

	if (nextPermutation(a, a + n))
		for (int i = 0; i < n; ++i)
			cout << a[i] << " ";
	else
		for (int i = 0; i < n; ++i)
			cout << "0 ";
	cout << "\n";

	return 0;
}
