#include <iostream>
#include <fstream>
using namespace std;
typedef long long LL;
struct _ { ios_base::Init i; _() { cin.sync_with_stdio(0); cin.tie(0); } } _;

int const maxn = 100000;

int a[maxn];

bool prevPermutation(int *begin, int *end)
{
	for (int *i = end - 1; begin < i; --i)
		if (*(i - 1) > *i)
		{
			int *j = i;
		 	--i;
			while (j < end && *i > *j)
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
	ifstream cin("nextperm.in");
	ofstream cout("nextperm.out");

	int n;
	cin >> n;
	for (int i = 0; i < n; ++i)
		cin >> a[i];

	if (prevPermutation(a, a + n))
		for (int i = 0; i < n; ++i)
			cout << a[i] << " ";
	else
		for (int i = 0; i < n; ++i)
			cout << "0 ";

	nextPermutation(a, a + n);
	cout << "\n";

	if (nextPermutation(a, a + n))
		for (int i = 0; i < n; ++i)
			cout << a[i] << " ";
	else
		for (int i = 0; i < n; ++i)
			cout << "0 ";

	return 0;
}
