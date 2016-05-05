#include <bits/stdc++.h>

#include "list.cpp"

#define print_list(name) \
std::cerr << "list " << #name << "\n";\
for (auto str : name)\
	std::cerr << str << "\n";\
std::cerr << "\n";

int main()
{
	list l;
	for (size_t i = 0; i < 10; ++i)
	{
		l.push_back("");
		for (size_t j = 0; j <= i; ++j)
			l.back() += '1';
	}

	print_list(l);

	list q;
	print_list(q);

	q = l;

	auto it = q.begin();
	for (size_t i = 0; i < 4; ++i)
		++it;
	it = q.slice(it, ++l.begin(), --l.end());
	q.insert(it, "qqq");

	print_list(q);

	print_list(l);

	return 0;
}
