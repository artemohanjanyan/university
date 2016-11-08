#include <bits/stdc++.h>
using namespace std;

int const MAX_N = 2000;
int const MAX_W = 100;

int current_n = 26;
vector<vector<int>> graph[MAX_N];
char starting_rule;

bool d[MAX_N][MAX_W][MAX_W];

bool processed[MAX_N][MAX_N];

bool is_small(int c)
{
	return c < 0;
}

void remove_long()
{
	int old_n = current_n;
	for (int i = 0; i < old_n; ++i)
		for (auto it = graph[i].begin(); it != graph[i].end(); ++it)
			while (it->size() > 2)
			{
				int last = it->back();
				it->pop_back();
				graph[current_n++] = {{it->back(), last}};
				it->back() = current_n - 1;
			}
}

bool accepts_epsilon = false;
void remove_epsilon()
{
	unordered_set<int> e_producing;
	for (int i = 0; i < current_n; ++i)
		for (auto const &rhs : graph[i])
			if (rhs.empty())
			{
				e_producing.insert(i);
				break;
			}

	bool has_changed = true;
	for (int i = 0; has_changed && i < current_n; ++i)
	{
		has_changed = false;
		for (int j = 0; j < current_n; ++j)
			if (e_producing.count(j) == 0)
				for (auto const &rhs : graph[j])
				{
					bool is_producing = true;
					for (auto c_it = rhs.begin(); is_producing && c_it != rhs.end(); ++c_it)
						is_producing &= !is_small(*c_it) && e_producing.count(*c_it);
					if (is_producing)
					{
						e_producing.insert(j);
						has_changed = true;
						break;
					}
				}
	}

	for (int i = 0; i < current_n; ++i)
	{
		vector<vector<int>> new_rules;
		for (auto const &rhs : graph[i])
		{
			int e_count = 0;
			for (auto const &c : rhs)
				if (!is_small(c) && e_producing.count(c))
					++e_count;
			int end_mask = (1 << e_count) - 1;
			for (int mask = 0; mask < end_mask; ++mask)
			{
				int tmp_mask = mask;
				vector<int> new_rhs;
				for (auto const &c : rhs)
					if (!is_small(c) && e_producing.count(c))
					{
						if (tmp_mask & 1)
							new_rhs.push_back(c);
						tmp_mask >>= 1;
					}
					else
						new_rhs.push_back(c);
				new_rules.push_back(move(new_rhs));
			}
		}
		for (auto const &new_rhs : new_rules)
			graph[i].push_back(move(new_rhs));
	}

	for (auto const &rhs : graph[starting_rule - 'A'])
		if (rhs.empty())
		{
			accepts_epsilon = true;
			break;
		}

	for (int i = 0; i < current_n; ++i)
		//if (i != static_cast<int>(starting_rule - 'A'))
		for (auto rhs_it = graph[i].begin(); rhs_it != graph[i].end(); )
			if (rhs_it->empty())
				rhs_it = graph[i].erase(rhs_it);
			else
				++rhs_it;
}

void remove_chain()
{
	for (int i = 0; i < current_n; ++i)
		for (int j = 0; j < static_cast<int>(graph[i].size()); ++j)
		{
			auto const &rhs = graph[i][j];
			int rhs_i = rhs[0];
			if (rhs.size() == 1 && !is_small(rhs_i) && rhs_i != i && !processed[i][rhs_i])
			{
				for (auto const &rhs2 : graph[rhs_i])
					graph[i].push_back(rhs2);
				processed[i][rhs_i] = true;
			}
		}
	for (int i = 0; i < current_n; ++i)
		for (auto rhs_it = graph[i].begin(); rhs_it != graph[i].end(); )
			if (rhs_it->size() == 1 && !is_small(rhs_it->at(0)) && rhs_it->at(0) != i)
				rhs_it = graph[i].erase(rhs_it);
			else
				++rhs_it;
}

void remove_useless()
{
	vector<bool> present(current_n);
	present[starting_rule - 'A'] = true;
	for (int i = 0; i < current_n; ++i)
		if (!graph[i].empty())
		{
			present[i] = true;
			for (auto const &rhs : graph[i])
				for (auto const &c : rhs)
					if (c >= 0)
						present[i] = true;
		}

	// Not producing

	vector<bool> producing(current_n);
	if (accepts_epsilon)
		producing[starting_rule - 'A'] = true;
	for (int i = 0; i < current_n; ++i)
		for (auto const &rhs : graph[i])
		{
			bool fl = true;
			for (auto const &c : rhs)
				fl &= is_small(c);
			if (fl)
			{
				producing[i] = true;
				break;
			}
		}

	for (int i = 0; i < current_n; ++i)
		for (int j = 0; j < current_n; ++j)
			if (!producing[j])
				for (auto const &rhs : graph[j])
				{
					bool fl = true;
					for (auto const &c : rhs)
						fl &= is_small(c) || producing[c];
					if (fl)
					{
						producing[j] = true;
						break;
					}
				}

	for (int i = 0; i < current_n; ++i)
		if (!producing[i])
			graph[i].clear();
		else
			for (auto it = graph[i].begin(); it != graph[i].end(); )
			{
				bool fl = true;
				for (auto const &c : *it)
					fl &= is_small(c) || producing[c];
				if (!fl)
					it = graph[i].erase(it);
				else
					++it;
			}

	// Not reachable

	vector<bool> reachable(current_n);
	reachable[starting_rule - 'A'] = true;

	for (int i = 0; i < current_n; ++i)
		for (int j = 0; j < current_n; ++j)
			if (reachable[j])
				for (auto const &rhs : graph[j])
					for (auto const &c : rhs)
						if (!is_small(c))
							reachable[c] = true;

	for (int i = 0; i < current_n; ++i)
		if (!reachable[i])
			graph[i].clear();
}

void remove_several_nonterm()
{
	for (int i = 0; i < current_n; ++i)
		for (auto &rhs : graph[i])
			if (rhs.size() == 2)
				for (auto &c : rhs)
					if (is_small(c))
					{
						graph[current_n++] = {{c}};
						c = current_n - 1;
					}
}

void print_graph()
{
	for (int i = 0; i < current_n; ++i)
		for (auto const &rhs : graph[i])
		{
			cerr << i << " -> ";
			for (int c : rhs)
				cerr << c << " ";
			cerr << "\n";
		}
	cerr << "\n";
}

int main()
{
	ifstream cin("cf.in");
	ofstream cout("cf.out");

	int m;
	cin >> m;
	cin >> starting_rule;
	for (int i = 0; i < m; ++i)
	{
		char lhs;
		string rhs;
		cin >> lhs;
		getline(cin, rhs);
		rhs = rhs.substr(min(4, (int) rhs.length()));
		vector<int> rhs_vec(rhs.begin(), rhs.end());
		for (int &c : rhs_vec)
			if (c >= static_cast<int>('a') && c <= static_cast<int>('z'))
				c = -(static_cast<int>(c) - static_cast<int>('a') + 1);
			else
				c = c - static_cast<int>('A');
		graph[lhs - 'A'].push_back(move(rhs_vec));
	}
	//print_graph();

	remove_long();
	//print_graph();
	remove_epsilon();
	//print_graph();
	remove_chain();
	//print_graph();
	remove_useless();
	//print_graph();
	remove_several_nonterm();
	//print_graph();

	string word;
	getline(cin, word);
	vector<int> word_vec(word.begin(), word.end());
	for (auto &c : word_vec)
		if (c >= static_cast<int>('a') && c <= static_cast<int>('z'))
			c = -(static_cast<int>(c) - static_cast<int>('a') + 1);
		else
			c = c - static_cast<int>('A');

	bool accepts = false;

	if (word.empty())
		accepts = accepts_epsilon;
	else
	{
		for (int i = 0; i < (int) word_vec.size(); ++i)
			for (int j = 0; j < current_n; ++j)
				for (auto const &rhs : graph[j])
					if (rhs.size() == 1 && rhs[0] == word_vec[i])
					{
						d[j][i][i] = true;
						break;
					}

		for (int i = 1; i < (int) word_vec.size(); ++i)
			for (int j = 0; j + i < (int) word_vec.size(); ++j)
			{
				int r = j + i;
				for (int lhs = 0; lhs < current_n; ++lhs)
					for (auto const &rhs : graph[lhs])
						if (rhs.size() == 2)
							for (int k = 0; k < r; ++k)
								d[lhs][j][r] |= d[rhs[0]][j][k] && d[rhs[1]][k + 1][r];
			}

		accepts = d[starting_rule - 'A'][0][word_vec.size() - 1];
	}

	if (accepts)
		cout << "yes\n";
	else
		cout << "no\n";

	return 0;
}
