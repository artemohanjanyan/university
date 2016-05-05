#include <bits/stdc++.h>

int const maxn = 400001;
int const alph = 'z' - 'a' + 2;
std::string s;
int shift[maxn], ord[maxn], count[maxn];
int newShift[maxn], newOrd[maxn];

using std::cin;
using std::cout;

struct node
{
	node *next[alph] = {};
	int from = 0;
	int to = 0;

	~node()
	{
		for (int i = 0; i < alph; ++i)
			if (this->next[i] != nullptr)
				delete this->next[i];
	}
};

int align(node *root)
{
	int maxDepth = 0;
	for (int i = 0; i < alph; ++i)
		if (root->next[i] != nullptr)
		{
			node *next = root->next[i];
			int depth = align(next);
			int length = next->to - next->from;
			next->from = ((int) s.length()) - depth - 1;
			next->to = next->from + length;
			maxDepth = std::max(maxDepth, depth);
		}
	return maxDepth + root->to - root->from;
}

void normalize(node *root, node *parent = nullptr)
{
	int lastI = -1, totalI = 0;
	for (int i = 0; i < alph; ++i)
		if (root->next[i] != nullptr)
		{
			lastI = i;
			++totalI;
			normalize(root->next[i], root);
		}
	if (totalI == 1 && parent != nullptr)
	{
		root->to = root->next[lastI]->to;
		auto oldNode = root->next[lastI];
		std::copy(oldNode->next, oldNode->next + alph, root->next);
		std::fill(oldNode->next, oldNode->next + alph, nullptr);
		delete oldNode;
	}
}

int getNodeN(node *root)
{
	int ans = 1;
	for (int i = 0; i < alph; ++i)
		if (root->next[i] != nullptr)
			ans += getNodeN(root->next[i]);
	return ans;
}

int globNode = 0;
void print(std::ostream &out, node *root, int p = -1)
{
	int curNode = ++globNode;
	if (p != -1)
		out << p << " " << curNode << " " << root->from + 1 << " " << root->to << "\n";
	for (int i = 0; i < alph; ++i)
		if (root->next[i] != nullptr)
			print(out, root->next[i], curNode);
}

int main()
{
	std::ifstream cin("tree.in");
	std::ofstream cout("tree.out");

	s.reserve(maxn);
	cin >> s;
	s.append("`");
	int n = (int) s.size();

	for (int i = 0; i < n; ++i)
		++count[s[i] - '`'];
	for (int i = 1; i < alph; ++i)
		count[i] += count[i - 1];
	for (int i = n - 1; i >= 0; --i)
		shift[--count[s[i] - '`']] = i;
	int ordN = 1;
	ord[shift[0]] = 0;
	for (int i = 1; i < n; ++i)
	{
		if (s[shift[i - 1]] != s[shift[i]])
			++ordN;
		ord[shift[i]] = ordN - 1;
	}

	for (int stepI = 0; (1 << stepI) < n; ++stepI)
	{
		for (int i = 0; i < n; ++i)
			shift[i] = (shift[i] - (1 << stepI) + n) % n;

		std::fill(count, count + n, 0);
		for (int i = 0; i < n; ++i)
			++count[ord[shift[i]]];
		for (int i = 1; i < n; ++i)
			count[i] += count[i - 1];
		for (int i = n - 1; i >= 0; --i)
			newShift[--count[ord[shift[i]]]] = shift[i];

		ordN = 1;
		newOrd[newShift[0]] = 0;
		for (int i = 1; i < n; ++i)
		{
			if (ord[newShift[i]] != ord[newShift[i - 1]] ||
					ord[(newShift[i] + (1 << stepI)) % n] != ord[(newShift[i - 1] + (1 << stepI)) % n])
				++ordN;
			newOrd[newShift[i]] = ordN - 1;
		}
		
		std::swap(shift, newShift);
		std::swap(ord, newOrd);
	}

	int *lcp = newShift, *revShift = newOrd;
	for (int i = 0; i < n; ++i)
		revShift[shift[i]] = i;
	for (int k = 0, i = 0; i < n; ++i)
	{
		if (k > 0)
			--k;
		if (revShift[i] == n - 1)
		{
			lcp[n - 1] = 0;
			k = 0;
		}
		else
		{
			int j = shift[revShift[i] + 1];
			while (std::max(i + k, j + k) < n && s[i + k] == s[j + k])
				++k;
			lcp[revShift[i]] = k;
		}
	}

	std::unique_ptr<node> tree{new node{}};
	std::vector<node*> stack = {tree.get()};
	int depth = 0;
	for (int i = 1; i < n; ++i)
	{
		//globNode = 0; print(cout, tree.get()); cout << "\n" << lcp[i - 1] << "\n";

		while (stack.size() > 1 && depth - (stack.back()->to - stack.back()->from) >= lcp[i - 1])
		{
			depth -= stack.back()->to - stack.back()->from;
			stack.pop_back();
		}

		if (depth != lcp[i - 1])
		{
			node *newNode = new node{};
			node *oldNode = stack.back();
			stack.pop_back();

			stack.back()->next[s[oldNode->from] - '`'] = newNode;

			newNode->from = oldNode->from;
			newNode->to = oldNode->to - (depth - lcp[i - 1]);
			newNode->next[s[newNode->to] - '`'] = oldNode;

			oldNode->from = newNode->to;

			depth -= oldNode->to - oldNode->from;
			stack.push_back(newNode);
		}

		node *newNode = new node{};
		newNode->from = shift[i] + lcp[i - 1];
		newNode->to = n - 1;

		stack.back()->next[s[newNode->from] - '`'] = newNode;

		stack.push_back(newNode);
		depth += newNode->to - newNode->from;
	}

	align(tree.get());
	normalize(tree.get());

	int nodeN = getNodeN(tree.get());
	cout << nodeN << " " << nodeN - 1 << "\n";
	print(cout, tree.get());

	return 0;
}
