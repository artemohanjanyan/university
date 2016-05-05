#include <bits/stdc++.h>
using namespace std;
struct _ { ios_base::Init i; _() { cin.sync_with_stdio(0); cin.tie(0); } } _;

int const maxn = 20000, maxm = 100000;

int parent[maxn], setRank[maxn];

void init(int n)
{
	fill(setRank, setRank + n, 1);
	fill(parent, parent + n, -1);
}

int getSet(int i)
{
	if (parent[i] == -1)
		return i;
	return parent[i] = getSet(parent[i]);
}

void unify(int a, int b)
{
	if (setRank[a] > setRank[b])
		swap(a, b);
	parent[a] = b;
	setRank[b] += setRank[a];
}

pair<int, pair<int, int>> edges[maxm];

int main()
{
	//ifstream cin("mindiff.in");
	//ofstream cout("mindiff.out");

	int n, m;
	cin >> n >> m;

	for (int i = 0; i < m; ++i)
	{
		cin >> edges[i].second.first >> edges[i].second.second >> 
				edges[i].first;
		--edges[i].second.first;
		--edges[i].second.second;
	}

	sort(edges, edges + m);
	
	int ans = numeric_limits<int>::max();

	for (int firstEdge = 0; firstEdge <= m - n + 1; ++firstEdge)
	{
		init(n);

		int curAns = 0, edgeN = 0;
		for (int i = firstEdge; i < m && edgeN < n - 1; ++i)
		{
			int setA = getSet(edges[i].second.first), setB = getSet(edges[i].second.second);
			if (setA != setB)
			{
				curAns += edges[i].first;
				unify(setA, setB);
				++edgeN;
				curAns = edges[i].first - edges[firstEdge].first;
			}
		}

		if (edgeN == n - 1)
			ans = min(curAns, ans);
	}

	if (ans != numeric_limits<int>::max())
		cout << "YES\n" << ans << "\n";
	else
		cout << "NO\n";

	return 0;
}
