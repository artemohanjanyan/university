#include <bits/stdc++.h>
using namespace std;

int const MAXN = 200000;

int a[MAXN], b[MAXN];

long long aAns[MAXN], bAns[MAXN];

int main()
{
	//ifstream cin("o2cmax.in");
	//ofstream cout("o2cmax.out");

	freopen("o2cmax.in", "r", stdin);
	freopen("o2cmax.out", "w", stdout);

	ios_base::sync_with_stdio(false);
	cin.tie(NULL);

	int n;
	cin >> n;
	for (int i = 0; i < n; ++i)
		cin >> a[i];
	for (int i = 0; i < n; ++i)
		cin >> b[i];

	vector<int> I, J;
	for (int i = 0; i < n; ++i)
		(a[i] <= b[i] ? I : J).push_back(i);

	auto xIt = max_element(I.begin(), I.end(), [&](int v1, int v2){ return a[v1] < a[v2]; });
	auto yIt = max_element(J.begin(), J.end(), [&](int v1, int v2){ return b[v1] < b[v2]; });

	int x;

	bool isReversed = false;

	if (xIt == I.end() || yIt == J.end() || a[*xIt] <= b[*yIt])
	{
		swap(a, b);

		I.clear();
		J.clear();

		for (int i = 0; i < n; ++i)
			(a[i] <= b[i] ? I : J).push_back(i);

		x = *max_element(I.begin(), I.end(), [&](int v1, int v2){ return a[v1] < a[v2]; });

		isReversed = true;
	}
	else
		x = *xIt;

	long long cmax = 0;
	for (int i = 0; i < n; ++i)
		cmax += a[i];
	long long tmpMax = 0;
	for (int i = 0; i < n; ++i)
		tmpMax += b[i];
	cmax = max(cmax, tmpMax);
	for (int i = 0; i < n; ++i)
		cmax = max(cmax, (long long) a[i] + b[i]);

	long long aTime = 0;
	for (int i = 0; i < (int) I.size(); ++i)
		if (I[i] != x)
		{
			aAns[I[i]] = aTime;
			aTime += a[I[i]];
		}
	bAns[x] = 0;
	aTime = b[x];
	for (int i = 0; i < (int) I.size(); ++i)
		if (I[i] != x)
		{
			bAns[I[i]] = aTime;
			aTime += b[I[i]];
		}

	aTime = a[x];
	aAns[x] = cmax - aTime;
	//for (int i = 0; i < (int) J.size(); ++i)
	for (int i = (int) J.size() - 1; i >= 0; --i)
	{
		aTime += a[J[i]];
		aAns[J[i]] = cmax - aTime;
	}
	aTime = 0;
	//for (int i = 0; i < (int) J.size(); ++i)
	for (int i = (int) J.size() - 1; i >= 0; --i)
	{
		aTime += b[J[i]];
		bAns[J[i]] = cmax - aTime;
	}

	if (isReversed)
		swap(aAns, bAns);

	cout << cmax << "\n";
	for (int i = 0; i < n; ++i)
		cout << aAns[i] << " ";
	cout << "\n";
	for (int i = 0; i < n; ++i)
		cout << bAns[i] << " ";
	cout << "\n";

	return 0;
}
