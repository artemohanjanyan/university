#include <bits/stdc++.h>
using namespace std;
struct _ { ios_base::Init i; _() { cin.sync_with_stdio(0); cin.tie(0); } } _;

int const maxn = 20000, maxm = 100000;

int parent[maxn], setRank[maxn];

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
	ifstream cin("spantree2.in");
	ofstream cout("spantree2.out");

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
	
	fill(setRank, setRank + n, 1);
	fill(parent, parent + n, -1);

	int ans = 0, edgeN = 0;
	for (int i = 0; edgeN < n - 1; ++i)
	{
		int setA = getSet(edges[i].second.first), setB = getSet(edges[i].second.second);
		if (setA != setB)
		{
			ans += edges[i].first;
			unify(setA, setB);
			++edgeN;
		}
	}

	cout << ans;

	return 0;
}
