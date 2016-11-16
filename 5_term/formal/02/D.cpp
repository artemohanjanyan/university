#include <bits/stdc++.h>
using namespace std;

int const MAX_N = 100000;
int const ALPH = 26;

int n;
unordered_map<int, int> graph[MAX_N];
bool is_term[MAX_N];

unordered_map<int, vector<int>> reverse_graph[MAX_N];
void build_reverse()
{
	for (int i = 0; i <= n; ++i)
		for (auto const &c : graph[i])
			reverse_graph[c.second][c.first].push_back(i);
}

bool reachable[MAX_N];
void find_reachable(int v = 0)
{
	reachable[v] = true;
	for (auto const &c : graph[v])
		if (!reachable[c.second])
			find_reachable(c.second);
}

bool usefull[MAX_N];
void find_usefull(int v)
{
	usefull[v] = true;
	for (auto const &vc : reverse_graph[v])
		for (auto const &c : vc.second)
			if (!usefull[c])
				find_usefull(c);
}

vector<shared_ptr<unordered_set<int>>> classes;
int class_index[MAX_N];
queue<pair<shared_ptr<unordered_set<int>>, int>> Queue;
void find_equivalence_classes()
{
	classes.push_back(make_shared<unordered_set<int>>());
	classes.push_back(make_shared<unordered_set<int>>());
	for (int i = 0; i < n; ++i)
		if (reachable[i] && usefull[i])
		{
			classes[is_term[i]]->insert(i);
			class_index[i] = is_term[i];
		}

	for (int c = 0; c < ALPH; ++c)
	{
		Queue.emplace(make_shared<unordered_set<int>>(*classes[0]), c);
		Queue.emplace(make_shared<unordered_set<int>>(*classes[1]), c);
	}

	while (!Queue.empty())
	{
		auto splitter = Queue.front();
		Queue.pop();
		unordered_map<int, vector<int>> involved;
		for (int q : *splitter.first)
			for (int r : reverse_graph[q][splitter.second])
				if (reachable[r] && usefull[r])
				{
					int i = class_index[r];
					if (!involved.count(i))
						involved.emplace(i, vector<int>{});
					involved[i].push_back(r);
				}
		for (auto const &i : involved)
			if (i.second.size() < classes[i.first]->size())
			{
				int j = static_cast<int>(classes.size());
				classes.push_back(make_shared<unordered_set<int>>());
				for (int r : involved[i.first])
				{
					classes[i.first]->erase(r);
					classes[j]->insert(r);
				}
				if (classes[j]->size() > classes[i.first]->size())
					swap(classes[j], classes[i.first]);
				for (int r : *classes[j])
					class_index[r] = j;
				decltype(auto) new_set = make_shared<unordered_set<int>>(*classes[j]);
				for (int c = 0; c < ALPH; ++c)
					Queue.emplace(new_set, c);
			}
	}

	int i = 0;
	int j = static_cast<int>(classes.size()) - 1;
	while (i < j)
	{
		while (i < j && !classes[i]->empty())
			++i;
		while (i < j && classes[j]->empty())
			--j;
		if (i >= j)
			break;

		swap(classes[i], classes[j]);
		for (int r : *classes[i])
			class_index[r] = i;

		++i;
		--j;
	}
	while (classes.back()->empty())
		classes.pop_back();

	if (class_index[0] != 0)
	{
		j = class_index[0];
		swap(classes[0], classes[j]);
		for (int r : *classes[0])
			class_index[r] = 0;
		for (int r : *classes[j])
			class_index[r] = j;
	}
}

int minimal_n, minimal_m, minimal_k;
unordered_map<int, int> minimal_graph[MAX_N];
bool minimal_is_term[MAX_N];
void build_minimal_graph()
{
	minimal_n = static_cast<int>(classes.size());
	for (int i = 0; i < n; ++i)
		if (reachable[i] && usefull[i])
		{
			minimal_is_term[class_index[i]] = is_term[i];
			for (auto const &c : graph[i])
				if (reachable[c.second] && usefull[c.second])
					minimal_graph[class_index[i]][c.first] = class_index[c.second];
		}
	minimal_k = static_cast<int>(count(minimal_is_term, minimal_is_term + minimal_n, true));
	for (int i = 0; i < minimal_n; ++i)
		minimal_m += static_cast<int>(minimal_graph[i].size());
}

int main()
{
	ifstream cin("fastminimization.in");
	ofstream cout("fastminimization.out");

	int m, k;
	cin >> n >> m >> k;
	for (int i = 0; i < k; ++i)
	{
		int v;
		cin >> v;
		is_term[v - 1] = true;
	}

	for (int i = 0; i < m; ++i)
	{
		int from, to;
		char c;
		cin >> from >> to >> c;
		graph[from - 1][static_cast<int>(c - 'a')] = to - 1;
	}

	build_reverse();
	find_reachable();
	for (int i = 0; i < n; ++i)
		if (is_term[i])
			find_usefull(i);
	find_equivalence_classes();
	build_minimal_graph();

	cout << minimal_n << " " << minimal_m << " " << minimal_k << "\n";
	for (int i = 0; i < minimal_n; ++i)
		if (minimal_is_term[i])
			cout << i + 1 << " ";
	cout << "\n";
	for (int i = 0; i < minimal_n; ++i)
		for (auto const &c : minimal_graph[i])
			cout << i + 1 << " " << c.second + 1 << " " << static_cast<char>('a' + c.first) << "\n";

	return 0;
}
