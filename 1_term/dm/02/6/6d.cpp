#include <bits/stdc++.h>
using namespace std;
typedef long long LL;
struct _ { ios_base::Init i; _() { cin.sync_with_stdio(0); cin.tie(0); } } _;

int main()
{
	ifstream cin("nextsetpartition.in");
	ofstream cout("nextsetpartition.out");

	int n, k;
	
	while (cin >> n >> k, !(n == 0 && k == 0))
	{
		vector<set<int>> part;
		string s;
		getline(cin, s);
		for (int i = 0; i < k; ++i)
		{
			part.push_back(set<int>());
			getline(cin, s);

			stringstream in;
			in << s;
			for (int next; in >> next; )
				part.rbegin()->insert(next);
		}

		bool fl = false;
		set<int> previous;
		for (int i = part.size() - 1; i >= 0 && !fl; --i)
			if (part[i].size() > 2 || !previous.empty() && *previous.rbegin() > *(part[i].rbegin()))
			{
				if (!previous.empty() && *previous.rbegin() > *part[i].rbegin())
				{
					auto it = previous.upper_bound(*part[i].rbegin());
					part[i].insert(*it);
					previous.erase(it);
				}
				else
				{
					auto first = *(++part[i].rbegin());

					previous.insert(*(--part[i].end()));
					part[i].erase(*(--part[i].end()));
					previous.insert(*(--part[i].end()));
					part[i].erase(*(--part[i].end()));

					auto it = previous.upper_bound(first);
					part[i].insert(*it);
					previous.erase(it);
				}

				set<int> tmp;
				for (auto elem : previous)
				{
					tmp.insert(elem);
					part.push_back(tmp);
					tmp.erase(tmp.begin());
				}
				
				fl = true;
			}
			else
			{
				for (auto elem : part[i])
					previous.insert(elem);
				part.pop_back();
			}

		if (fl)
		{
			cout << n << " " << part.size() << "\n";
			for (auto partSet : part)
			{
				for (auto elem : partSet)
					cout << elem << " ";
				cout << "\n";
			}
		}
		else
		{
			cout << n << " " << n << "\n";
			for (int i = 0; i < n; ++i)
				cout << i + 1 << "\n";
		}
		cout << "\n";
	}

	return 0;
}
