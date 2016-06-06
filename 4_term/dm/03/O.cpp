#include <bits/stdc++.h>
using namespace std;
struct _ { ios_base::Init i; _() { cin.sync_with_stdio(0); cin.tie(0); } } _;

int const MAXN = 50000;
int const MAXM = 10000;

pair<int, long long> ans[MAXN];
long long times[MAXM];

priority_queue<pair<int, int>> jobs;

struct machine_type
{
	long long k;
	int t, i;
};

class rational_comp
{
public:
	bool operator()(machine_type const &a, machine_type const &b)
	{
		return a.k * a.t > b.k * b.t || (a.k * b.t == b.k * a.t && a.i > b.i);
	}
};

priority_queue<machine_type, vector<machine_type>, rational_comp> machines;

int main()
{
	//freopen("qsumci.in", "r", stdin);
	//freopen("qsumci.out", "w", stdout);
	//ios_base::sync_with_stdio(false);
	//cin.tie(NULL);

	int n, m;
	cin >> n >> m;
	for (int i = 0; i < n; ++i)
	{
		int p;
		cin >> p;
		jobs.push(make_pair(p, i));
	}
	for (int i = 0; i < m; ++i)
	{
		int t;
		cin >> t;
		machines.push({1, t, i});
	}

	long long total_penalty = 0;
	while (!jobs.empty())
	{
		auto job = jobs.top();
		jobs.pop();
		auto machine = machines.top();
		machines.pop();

		int current_time = machine.t * job.first;
		total_penalty += machine.k * current_time;
		times[machine.i] -= current_time;
		ans[job.second] = {machine.i, times[machine.i]};

		machines.push({machine.k + 1, machine.t, machine.i});
	}

	cout << total_penalty << "\n";
	for (int i = 0; i < n; ++i)
		cout << ans[i].first + 1 << " " << -times[ans[i].first] + ans[i].second << "\n";

	return 0;
}
