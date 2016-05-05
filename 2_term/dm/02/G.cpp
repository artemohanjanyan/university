#include <bits/stdc++.h>
using namespace std;
struct _ { ios_base::Init i; _() { cin.sync_with_stdio(0); cin.tie(0); } } _;

#define minn(a, b) ((a) < (b) ? (a) : (b))

const size_t SIZE = 30000000;
const int32_t MAX = numeric_limits<int32_t>::max();

size_t size1 = 0, size2 = 0;
int32_t elements[SIZE];
int32_t minns[SIZE];

void push(int32_t elem)
{
	elements[size1++] = elem;
	minns[size1 - 1] = (size1 == 1 ? elem : minn(elem, minns[size1 - 2]));
}

void pop()
{
	if (size2 == 0)
		while (size1 > 0)
		{
			elements[SIZE - (++size2)] = elements[--size1];
			minns[SIZE - size2] = (size2 == 1 ?
				elements[SIZE - size2] :
				minn(elements[SIZE - size2], minns[SIZE - size2 + 1]));
		}

	--size2;
}

int32_t queueMin()
{
	if (size1 == 0)
		return minns[SIZE - size2];
	else if (size2 == 0)
		return minns[size1 - 1];
	else
		return minn(minns[size1 - 1], minns[SIZE - size2]);
}

int32_t a, b, c;
size_t k;
int32_t *x;
int32_t getNext()
{
	static size_t i = 0;
	if (i < k)
		return x[i++];
	else
	{
		x[i % k] = a * x[(i - 2 + k) % k] + b * x[(i - 1 + k) % k] + c;
		return x[(i++ % k)];
	}
}

int main()
{
	//ifstream cin("queuemin2.in");
	//ofstream cout("queuemin2.out");

	size_t n, m;
	cin >> n >> m >> k;
	cin >> a >> b >> c;
	x = new int32_t[k];
	for (size_t i = 0; i < k; ++i)
		cin >> x[i];

	for (size_t i = 0; i < m - 1; ++i)
		push(getNext());

	int64_t ans = 0;
	for (size_t i = m - 1; i < n; ++i)
	{
		push(getNext());
		ans += queueMin();
		pop();
	}

	cout << ans << endl;

	return 0;
}
