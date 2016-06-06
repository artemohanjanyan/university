#include <bits/stdc++.h>
using namespace std;
struct _ { ios_base::Init i; _() { cin.sync_with_stdio(0); cin.tie(0); } } _;

int const MAXN = 100000;

vector<int> deps[MAXN];
int parents[MAXN];
int d[MAXN];
int dd[MAXN];
int sorted[MAXN];

queue<int> q;

int dep_max[MAXN];
int start_time[MAXN];
int jobs[MAXN + 1];

int main()
{
	freopen("pintreep1l.in", "r", stdin);
	freopen("pintreep1l.out", "w", stdout);
	ios_base::sync_with_stdio(false);
	cin.tie(NULL);

	int n, m;
	cin >> n >> m;
	fill(parents, parents + n, -1);
	for (int i = 0; i < n; ++i)
		cin >> d[i];
	copy(d, d + n, dd);
	for (int i = 1; i < n; ++i)
	{
		int before, after;
		cin >> before >> after;
		--before;
		--after;
		deps[after].push_back(before);
		parents[before] = after;
	}

	q.push(static_cast<int>(find(parents, parents + n, -1) - parents));

	while (!q.empty())
	{
		int v = q.front();
		q.pop();
		for (int child : deps[v])
		{
			d[child] = min(d[child], d[v] - 1);
			q.push(child);
		}
	}

	for (int i = 0; i < n; ++i)
		sorted[i] = i;
	sort(sorted, sorted + n, [&](int a, int b){ return d[a] < d[b];});

	int earliest = 0;
	fill(dep_max, dep_max + n, 0);
	fill(jobs, jobs + n + 1, 0);
	for (int i = 0; i < n; ++i)
	{
		int current = max(dep_max[sorted[i]], earliest);
		start_time[sorted[i]] = current;
		if (++jobs[current] == m)
			earliest = current + 1;
		int parent = parents[sorted[i]];
		if (parent != -1)
			dep_max[parent] = max(dep_max[parent], current + 1);
	}

	int ans = INT_MIN;
	for (int i = 0; i < n; ++i)
		ans = max(start_time[i] - dd[i] + 1, ans);
	cout << ans << "\n";
	for (int i = 0; i < n; ++i)
		cout << start_time[i] << " ";

	return 0;
}
