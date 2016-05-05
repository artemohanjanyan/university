#include <bits/stdc++.h>
using namespace std;
typedef long long LL;
struct _ { ios_base::Init i; _() { cin.sync_with_stdio(0); cin.tie(0); } } _;

const int maxn = 1000, maxw = 10000;

int dp[maxn + 1][maxw + 1], p[maxn + 1][maxw + 1], weights[maxn], costs[maxn];

int main()
{
	ifstream cin("knapsack.in");
	ofstream cout("knapsack.out");

	int n, maxWeight;
	cin >> n >> maxWeight;
	for (int i = 0; i < n; ++i)
		cin >> weights[i];
	for (int i = 0; i < n; ++i)
		cin >> costs[i];

	for (int i = 0; i < n; ++i)
		for (int w = 1; w <= maxWeight; ++w)
		{
			dp[i + 1][w] = dp[i + 1][w - 1];
			p[i + 1][w] = 0;

			if (dp[i][w] > dp[i + 1][w])
			{
				dp[i + 1][w] = dp[i][w];
				p[i + 1][w] = 1;
			}

			if (w >= weights[i] && dp[i][w - weights[i]] + costs[i] > dp[i + 1][w])
			{
				dp[i + 1][w] = dp[i][w - weights[i]] + costs[i];
				p[i + 1][w] = 2;
			}
		}

	vector<int> ans;
	//cerr << endl;
	int ansI = n, ansJ = maxWeight;
	while (ansI > 0 && ansJ > 0)
	{
		//cerr << ansI << " " << ansJ << endl;
		if (p[ansI][ansJ] == 2)
			ans.push_back(ansI);

		int curP = p[ansI][ansJ];

		if (curP == 0)
			--ansJ;
		else if (curP == 1)
			--ansI;
		else
			ansJ -= weights[--ansI];
	}

	//cerr << endl;
	//for (int i = 0; i <= n; ++i)
	//{
	//	for (int j = 0; j <= maxWeight; ++j)
	//		cerr << dp[i][j] << " ";
	//	cerr << endl;
	//}
	//cerr << endl;

	//cerr << endl;
	//for (int i = 0; i <= n; ++i)
	//{
	//	for (int j = 0; j <= maxWeight; ++j)
	//		cerr << p[i][j] << " ";
	//	cerr << endl;
	//}
	//cerr << endl;

	cout << ans.size() << "\n";
	for (auto it = ans.rbegin(); it != ans.rend(); ++it)
		cout << *it << " ";
	cout << "\n";

	return 0;
}
