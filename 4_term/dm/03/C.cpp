#include <bits/stdc++.h>
using namespace std;

int const maxn = 50000;

int p[maxn], w[maxn];
int parents[maxn];

vector<int> graph[maxn];

int E[maxn];
set<int> J[maxn];

int parI[maxn];

class comp
{
public:
	bool operator()(int a, int b)
	{
		return ((long long) w[a]) * p[b] < ((long long) w[b]) * p[a];
	}
};

set<int, comp> L;

vector<int> children[maxn];

int pp[maxn];
int ww[maxn];

int cur_time = 0;
long long get_ans(int i)
{
	cur_time += pp[i];
	long long ans = cur_time * ww[i];
	for (int q : children[i])
		ans += get_ans(q);
	return ans;
}

void print_ans(ostream &out, int i)
{
	out << i + 1 << " ";
	for (int q : children[i])
		print_ans(out, q);
}

int main()
{
	int n;
	cin >> n;
	for (int i = 0; i < n; ++i)
	{
		cin >> p[i];
		pp[i] = p[i];
	}
	for (int i = 0; i < n; ++i)
	{
		cin >> w[i];
		ww[i] = w[i];
	}

	fill(parents, parents + n, -1);

	for (int i = 1; i < n; ++i)
	{
		int u, v;
		cin >> u >> v;
		graph[v - 1].push_back(u - 1);
		parents[u - 1] = v - 1;
	}

	for (int i = 0; i < n; ++i)
		parI[i] = i;

	int root = (int) (find(parents, parents + n, -1) - parents);
	w[root] = -1000000;

	for (int i = 0; i < n; ++i)
		L.insert(i);

	while (L.size() > 1)
	{
		auto it = prev(L.end());
		if (*it == root)
			--it;
		int j = *it;
		int parent = parents[j];
		int i = parI[parent];

		L.erase(i);
		w[i] = w[i] + w[j];
		p[i] = p[i] + p[j];
		L.insert(i);

		parents[j] = E[i];

		for (int k : J[j])
			parI[k] = j;

		children[i].push_back(j);

		if (J[i].size() > J[j].size())
		{
			for (int x : J[j])
				J[i].insert(x);
			J[j].clear();
		}
		else
		{
			for (int x : J[i])
				J[j].insert(x);
			J[i].clear();
			J[i].swap(J[j]);
		}

		L.erase(it);
	}

	cout << get_ans(*L.begin()) << "\n";
	print_ans(cout, *L.begin());

	return 0;
}
